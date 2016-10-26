{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Stackage.Package.Git.Repository where

import ClassyPrelude.Conduit
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Git hiding (Commit, Tag)
import Data.Git.Types
import Data.Git.Repository
import Data.Git.Storage.Object
import Data.Git.Storage.Loose
import qualified Filesystem.Path.CurrentOS as P
import Data.Conduit.Process
       (withCheckedProcessCleanup, sourceProcessWithStreams,
        Inherited(Inherited))
import qualified Data.HashTable.IO as H
import ClassyPrelude.Conduit (sourceLazy, sinkLazyBuilder)
import System.Directory
import System.Exit
import System.FilePath
import System.Process (proc, cwd, showCommandForUser)

import Stackage.Package.Git.Types
import Stackage.Package.Git.Object
import Stackage.Package.Git.WorkTree

withRepository :: GitInfo -> (GitRepository -> IO a) -> IO a
withRepository info@GitInfo {..} action = do
  properRepo <- isRepo (P.decodeString gitLocalPath)
  unless properRepo $ error $ "There is no git repository in :" ++ gitLocalPath
  withRepo (P.decodeString gitLocalPath) $
    \git -> do
      branchRef <-
        maybe
          (error $
           "Cannot resolve " ++ gitBranchName ++ " for repository: " ++ gitLocalPath)
          id <$>
        resolveRevision git (fromString gitBranchName)
      headSet git (Right (RefName gitBranchName))
      workTree <- H.new
      objectsTable <- getObjectsTable gitLocalPath
      action
        GitRepository
        { repoInstance =
          GitInstance
          { gitRepo = git
          , gitBranchRef = branchRef
          , gitWorkTree = workTree
          , gitObjects = objectsTable
          }
        , repoInfo = info
        }


getObjectsTable :: FilePath -> IO ObjectsTable
getObjectsTable localPath = do
  objectsTable <- H.new
  let insertSHA =
        lineAsciiC $
        takeCE 40 .| decodeBase16C .|
        (foldC >>= bsToShortRef >>= (\k -> liftIO $ H.insert objectsTable k ()))
  void $
    runPipeWithConduit
      localPath
      "git"
      ["rev-list", "--objects", "--all"]
      (sourceLazy "")
      (peekForever insertSHA)
      sinkNull
  return objectsTable


repoReadFile :: GitRepository -> FilePath -> IO (Maybe LByteString)
repoReadFile repo@GitRepository {repoInstance = GitInstance {..}} fp = do
  mnewRef <- lookupFileRef gitWorkTree (toTreePath fp)
  {- check work tree first, that file might have been updated. -}
  (isNew, mref) <-
    case mnewRef of
      Just newRef -> return (True, Just newRef)
      Nothing -> do
        mRef <-
          resolvePath
            gitRepo
            gitBranchRef
            (map (entName . S8.pack) $ splitDirectories fp)
        return (False, mRef)
  case mref of
    Just ref -> do
      mobj <- getObject gitRepo ref True
      case mobj of
        Just (ObjBlob (Blob blob))
        {- if we are reading a file, then we expect it to be present in a work tree too. -}
         -> do
          unless isNew $ insertRef repo (toTreePath fp) ref
          return $ Just blob
        _ -> return Nothing
    Nothing -> return Nothing



-- | Same as `readRepoFile`, but will raise an error if file cannot be found.
repoReadFile' :: GitRepository -> FilePath -> IO LByteString
repoReadFile' repo@GitRepository {repoInfo = GitInfo {..}} fp = do
  mfile <- repoReadFile repo fp
  case mfile of
    Just file -> return file
    Nothing ->
      error $
      "Could not find file: " ++
      fp ++ " in repository: " ++ gitLocalPath ++ " under branch: " ++ gitBranchName



repoWriteFile :: GitRepository -> FilePath -> LByteString -> IO ()
repoWriteFile repo fp f = do
  ref <- persistBlob repo f
  insertRef repo (toTreePath fp) ref



repoCreateCommit :: GitRepository -> ByteString -> IO (Maybe Ref)
repoCreateCommit repo@GitRepository {repoInstance = GitInstance {..}
                                    ,repoInfo = GitInfo {..}} msg = do
  rootTreeRef <- flushTree repo
  mtreeRef <- resolvePath gitRepo gitBranchRef []
  case mtreeRef of
    Nothing -> error "Could not resolve root tree from current branch"
    Just treeRef
      | treeRef == rootTreeRef -> do
        putStrLn "Nothing to commit"
        return Nothing
    Just _ -> do
      commit <- makeGitCommit rootTreeRef [gitBranchRef] gitUser msg
      newCommit <-
        case userGpgKey gitUser of
          Just key -> signCommit gitRepo key commit
          Nothing -> return commit
      commitRef <- setObject gitRepo (ObjCommit newCommit)
      branchWrite gitRepo (RefName gitBranchName) commitRef
      return (Just commitRef)


repoCreateTag :: GitRepository -> Ref -> ByteString -> IO (Maybe Ref)
repoCreateTag repo@GitRepository {repoInstance = GitInstance {..}
                                 ,repoInfo = GitInfo {gitTagName = Just tagName
                                                     ,..}} commitRef tagMessage = do
  tag <- makeGitTag commitRef gitUser tagName tagMessage
  newTag <-
    case userGpgKey gitUser of
      Just key -> signTag gitRepo key tag
      Nothing -> return tag
  ref <- setObject gitRepo (ObjTag newTag)
  writeFile (gitLocalPath </> "refs" </> "tags" </> S8.unpack (tagBlob newTag)) (show ref)
  return $ Just ref
repoCreateTag _ _ _ = return Nothing




-- | Clones the repo if it doesn't exists locally yet, otherwise fetches and
-- creates a local branch.
ensureRepository
  :: String -- ^ Git provider ex. "github.com"
  -> String -- ^ Repository account ex. "commercialhaskell"
  -> GitUser -- ^ User information to be used for the commits.
  -> String -- ^ Repository name ex. "all-cabal-files"
  -> String -- ^ Repository branch ex. "master"
  -> FilePath -- ^ Location in the file system where
     -- repository should be cloned to.
  -> IO GitInfo
ensureRepository repoHost repoAccount gitUser repoName repoBranchName repoBasePath = do
  exists <- doesDirectoryExist repoLocalPath
  if exists
    then do
      run repoLocalPath "git" ["remote", "set-url", "origin", repoAddress]
      run repoLocalPath "git" ["fetch"]
    else run
           "."
           "git"
           [ "clone"
           , "--bare"
           , "--depth=1"
           , repoAddress
           , repoLocalPath
           , "--branch"
           , repoBranchName             
           ]
  return
    GitInfo
    { gitAddress = repoAddress
    , gitBranchName = repoBranchName
    , gitUser = gitUser
    , gitTagName = Nothing
    , gitLocalPath = repoLocalPath
    }
  where
    repoLocalPath = repoBasePath </> repoName
    repoAddress =
      concat ["git@", repoHost, ":", repoAccount, "/", repoName, ".git"]



signCommit :: Git -> String -> Commit -> IO Commit
signCommit gitRepo key commit = do
  signature <- signObject gitRepo key (ObjCommit commit)
  -- Workaround for: https://github.com/vincenthz/hit/issues/35
  -- Otherwise should be:
  -- let signatureKey = "gpgsig"
  --     signature = L.toStrict out
  let signatureKey = L.toStrict $ L.append "gpgsig " $ L8.takeWhile (/='\n') signature
      signature' = L.toStrict $ L.tail $ L8.dropWhile (/='\n') signature
  return
    commit
    { commitExtras = commitExtras commit ++ [CommitExtra signatureKey signature']
    }


signTag :: Git -> String -> Tag -> IO Tag
signTag gitRepo key tag = do
  signature <- signObject gitRepo key (ObjTag tag)
  return
    tag
    { tagS = S8.intercalate "\n" [tagS tag, L.toStrict signature]
    }


signObject :: Git -> String -> Object -> IO L.ByteString
signObject gitRepo key obj = do
  gpgProgram <- maybe "gpg" id <$> configGet gitRepo "gpg" "program"
  let args = ["-bsau", key]
  let payload = L.tail $ L.dropWhile (/= 0) $ looseMarshall obj
  (signature, err) <- runPipe "." gpgProgram args payload
  unless (L.null err) $ putStrLn $ "Warning: " ++ pack (L8.unpack err)
  return signature



---------------------------------------------------
-- Helper functions, that run external processes --
---------------------------------------------------

-- | Run an external process.
run
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> IO ()
run dir cmd args = do
  putStrLn $ concatMap pack ["Running in ", dir, ": ", showCommandForUser cmd args]
  withCheckedProcessCleanup
    (proc cmd args)
    { cwd = Just dir
    }
    (\Inherited Inherited Inherited -> return ())


-- | Run an external process, pipes @stdin@ to it and returns @(stdout,
-- stderr)@. Throws an error if process exited abnormally.
runPipe
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> LByteString -- ^ @stdin@
  -> IO (LByteString, LByteString)
runPipe dir cmd args input =
  runPipeWithConduit dir cmd args (sourceLazy input) sinkLazyBuilder sinkLazyBuilder


-- | Run an external process, pipes @stdin@ to it and returns @(stdout,
-- stderr)@. Throws an error if process exited abnormally.
runPipeWithConduit
  :: FilePath -- ^ Filepath where process will run.
  -> FilePath -- ^ Path to executable
  -> [String] -- ^ Arguments
  -> Producer IO ByteString
  -> Consumer ByteString IO a
  -> Consumer ByteString IO b
  -> IO (a, b)
runPipeWithConduit dir cmd args inputSrc stdoutSink stderrSink = do
  putStrLn $
    concatMap pack ["Running in ", dir, ": ", showCommandForUser cmd args]
  (exitCode, out, err) <-
    sourceProcessWithStreams
      (proc cmd args)
      { cwd = Just dir
      }
      inputSrc
      stdoutSink
      stderrSink
  case exitCode of
    ExitSuccess -> return (out, err)
    code ->
      error $
      "Stackage.Package.Location.runPipeWithConduit: " ++
      showCommandForUser cmd args ++ " produced an error: " ++ show code
