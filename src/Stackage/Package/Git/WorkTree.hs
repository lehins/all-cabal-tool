{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Stackage.Package.Git.WorkTree
  ( emptyWorkTree
  , readWorkTree
  , flushWorkTree
  , insertGitFile
  , removeGitFile
  , lookupFile
  , showWorkTree
  ) where

import ClassyPrelude.Conduit
import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as S8
import Data.Byteable
import qualified Data.Git as G
import Data.Git.Ref
import Data.Git.Storage
import qualified Data.Map.Strict as Map
import qualified Data.Tree as T

import Stackage.Package.Git.Types
import Stackage.Package.Git.Object


-- | Convert a work tree directory into a Tree object.
directoryToTree :: Map.Map FileName (WorkTree ShortRef ShortRef) -> G.Tree
directoryToTree dirMap = G.Tree $ map toEnt' (Map.toAscList dirMap)
  where
    toEnt' (name, t) = (getTreeMode t, toEntName name, fromShortRef $ getTreeRef t)
    toEntName (FileName fName) = G.entName $ BS.fromShort fName
    toEntName (DirectoryName dirName) = G.entName $ S.init $ BS.fromShort dirName


-- | Empty work tree.
emptyWorkTree :: WorkTree () GitFile
emptyWorkTree = Directory () Map.empty


-- | Places a file into a work tree. Here is the resolution upon a conflict of names:
--
-- * File name clash with either a folder or a file will effectively replace
--   it. (e.g. inserting file @foo/bar@ will remove either a file @foo/bar@ or a
--   directory @foo/bar/...@)
--
-- * Directory clash with a file will also replace that file (e.g. inserting
--   @foo/bar/baz.txt@ will remove a file @foo/bar@)
--
-- * Naturally, a clash of a directory with a directory will result in their merge.
--
insertGitFile :: WorkTree () GitFile -> TreePath -> GitFile -> WorkTree () GitFile
insertGitFile tree path f = insertFileRec tree path
  where
    insertFileRec _ [] = error "Cannot insert a file without a name"
    insertFileRec !(Directory _ dirMap) [fName] =
      Directory () $ Map.insert fName (File f $ gitFileType f) dirMap
    insertFileRec !(Directory _ dirMap) (dirName:pathTail) =
      Directory () $ Map.alter with dirName dirMap
      where
        with (Just subTree@(Directory _ _)) = Just $ insertFileRec subTree pathTail
        with _ = Just $ insertFileRec (Directory () Map.empty) pathTail
    insertFileRec !(File {}) treePath = insertFileRec (Directory () Map.empty) treePath


-- | Returns a file from a work tree if one extst at a supplied path.
lookupFile :: WorkTree a f -> TreePath -> Maybe f
lookupFile tree path = getFile $ lookupRec tree path
  where
    getFile (Just (File f _)) = Just f
    getFile _ = Nothing
    lookupRec _ [] = error "Cannot lookup a file without a name."
    lookupRec (Directory _ dirMap) [fileName] = Map.lookup fileName dirMap
    lookupRec (Directory _ dirMap) (dirName:pathTail) =
      case Map.lookup dirName dirMap of
        Just subTree -> lookupRec subTree pathTail
        Nothing -> Nothing
    lookupRec File {} _ = Nothing


removeGitFile :: WorkTree () GitFile -> TreePath -> WorkTree () GitFile
removeGitFile = removeRec
  where
    removeRec _ [] = error "Cannot remove a file without a name."
    removeRec !(Directory _ dirMap) [fileName] =
      Directory () $ Map.delete fileName dirMap
    removeRec !(Directory _ dirMap) (dirName:pathTail) =
      Directory () $ Map.update (Just . (`removeRec` pathTail)) dirName dirMap
    removeRec f@(File {}) _ = f


-- | Recursively reads current state of the tree.
readWorkTree :: Git -- ^ Git Repo.
             -> Ref -- ^ Reference for the root tree.
             -> IO (WorkTree ShortRef ShortRef)
readWorkTree !repo !rootRef = readTreeRec rootRef
  where
    readTreeFile !(G.ModePerm mode, ent, ref)
      | mode == 0o120000 = do
        sRef <- toShortRef ref
        return (FileName $! BS.toShort $! toBytes ent, File sRef SymLink)
      | mode == 0o160000 = do
        sRef <- toShortRef ref
        return (FileName $! BS.toShort $! toBytes ent, File sRef GitLink)
      | (mode .&. 0o777000) == 0o100000 = do
        let !fileType =
              case mode .&. 0o777 of
                0o755 -> ExecFile
                0o644 -> NonExecFile
                0o664 -> NonExecGroupFile
                _ -> error $ "Unrecognized file type: " ++ show mode
        sRef <- toShortRef ref
        return (FileName $! BS.toShort $! toBytes ent, File sRef fileType)
      | mode == 0o040000 = do
        directory <- readTreeRec ref
        return
          (DirectoryName $! BS.toShort $! S8.snoc (toBytes ent) '/', directory)
      | otherwise = error $ "Unsupported file mode: " ++ show mode
    readTreeRec !ref = do
      G.Tree tree <- G.getTree repo ref
      files <- mapM readTreeFile tree
      sRef <- toShortRef ref
      return $! Directory sRef $! Map.fromAscList files


-- | Writes a single directory to disk.
persistGitTree
  :: GitRepository
  -> Map.Map FileName (WorkTree ShortRef ShortRef)
  -> IO (WorkTree ShortRef ShortRef)
persistGitTree !repo !dirMap = do
  ref <- repoWriteObject repo (Tree $ directoryToTree dirMap)
  sRef <- toShortRef ref
  return $! Directory sRef dirMap


-- | Recursively writes a work tree to disk
diveTreePersist :: GitRepository -> WorkTree () GitFile -> IO (WorkTree ShortRef ShortRef)
diveTreePersist !repo !(File f t) = do
  ref <- repoWriteObject repo (Blob f)
  sRef <- toShortRef ref
  return $! File sRef t
diveTreePersist !repo !(Directory _ dirMap) = do
  let !dir = Map.toAscList dirMap
  persistedTrees <- mapM (diveTreePersist repo . snd) dir
  let persistedMap = Map.fromAscList $ zip (map fst dir) persistedTrees
  persistGitTree repo persistedMap


-- | Merges current root tree with a work tree, while writing out to disk any
-- files that have changed, as well as a changed tree structure. At the end,
-- changes are written to disk, current `gitWorkTree` is updated, and refernce
-- for new root tree is returned, unless no changes were detected, then there
-- will be no hard drive IO and `Nothing` is returned.
flushWorkTree :: GitRepository -> IO (Maybe Ref)
flushWorkTree !repo@GitRepository {repoInstance = GitInstance {..}} = do
  modifyMVar gitWorkTree $
    \(rootTree, workTree) -> do
      newRootTree <- flushWorkTreeRec rootTree workTree
      let mnewRootRef =
            if getTreeRef newRootTree == getTreeRef rootTree
              then Nothing
              else Just $ fromShortRef (getTreeRef newRootTree)
      return ((newRootTree, emptyWorkTree), mnewRootRef)
  where
    flushWorkTreeRec !(File ref _) !file@(File f t2) = do
      sRef <- toShortRef (gitFileRef f)
      if ref == sRef then return (File sRef t2) else diveTreePersist repo file
    flushWorkTreeRec !(Directory {}) !file@(File _ _) = do
      diveTreePersist repo file
    flushWorkTreeRec !(File {}) !d2@(Directory _ _) = do
      diveTreePersist repo d2
    flushWorkTreeRec !d1@(Directory _ dirMap1) !(Directory _ dirMap2)
      | Map.null dirMap2 = return d1
      | otherwise = do
        let unchangedMap = Map.difference dirMap1 dirMap2
            newMap = Map.difference dirMap2 dirMap1
            newList = Map.toAscList newMap
        newFiles <- mapM (diveTreePersist repo . snd) newList
        let distinctMap =
              Map.union unchangedMap $ Map.fromAscList $ zip (map fst newList) newFiles
            oldDirs = Map.toAscList $ Map.difference dirMap1 unchangedMap
            changedDirs = Map.toAscList $ Map.difference dirMap2 newMap
        commonDirs <-
          zipWithM flushWorkTreeRec (map snd oldDirs) (map snd changedDirs)
        let updatedMap = Map.fromAscList $ zip (map fst changedDirs) commonDirs
        let newDirMap = Map.union distinctMap updatedMap
        if newDirMap == dirMap1
          then return d1
          else persistGitTree repo newDirMap


workTreeToTree :: String -> WorkTree () GitFile -> T.Tree String
workTreeToTree parentName (File gf _) =
  T.Node (parentName ++ " with Ref: " ++ show (gitFileRef gf)) []
workTreeToTree parentName (Directory _ dirMap) =
  T.Node parentName $
  map (\(k, v) -> workTreeToTree (show k) v) $
  Map.toAscList dirMap


-- | Show tree in a readable format. Useful for debugging.
showWorkTree :: WorkTree () GitFile -> String
showWorkTree = T.drawTree . workTreeToTree "/"
