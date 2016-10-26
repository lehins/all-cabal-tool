{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Package.Git.Types where

import ClassyPrelude.Conduit
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as BU
import Data.Git.Ref
import qualified Data.Map as Map
import qualified Data.HashTable.IO as H
import Data.Word (Word32)
import Data.Hourglass (timeFromElapsed)
import Foreign.Storable (peekByteOff)
import Time.System (timeCurrent)

import qualified Data.Git as G


data GitUser = GitUser
  { userName :: ByteString
  , userEmail :: ByteString
  , userGpgKey :: Maybe String
  } deriving (Show)



data GitInfo = GitInfo
  { gitAddress :: String
    -- ^ Git address of the repository were it can be cloned from using SSH key.
  , gitBranchName :: String
    -- ^ Branch that updates should be committed to.
  , gitUser :: GitUser
    -- ^ User information to be used for the commits.
  , gitTagName :: Maybe ByteString
    -- ^ Create a tag after an update.
  , gitLocalPath :: FilePath
  }

type WorkTree = H.BasicHashTable FileName TreeEntry

type ObjectsTable = H.BasicHashTable ShortRef ()

data TreeEntry
  = TEDir !WorkTree
  | TEFile !G.Ref

data GitInstance = GitInstance
  { gitRepo :: G.Git
    -- ^ Git repository instance.
  , gitBranchRef :: G.Ref
    -- ^ Current work tree written to the git store.
  , gitObjects :: !ObjectsTable
  , gitWorkTree :: !WorkTree
  }


data GitRepository = GitRepository
  { repoInstance :: GitInstance
  , repoInfo :: GitInfo
  }


data FileName
  = FileName !BS.ShortByteString
  | DirectoryName !BS.ShortByteString
  deriving (Show)


bsFileName :: FileName -> ByteString
bsFileName !(FileName fName) = BS.fromShort fName
bsFileName !(DirectoryName dirName) = S8.init $ BS.fromShort dirName


type TreePath = [FileName]

toTreePath :: FilePath -> TreePath
toTreePath path = foldr toFileName [] splitPath'
  where
    splitPath' = S8.split '/' $ S8.pack path
    toFileName fName []
      | null fName =
        error $ "Tree path should end with a file name, not a directory: " ++ path
      | otherwise = [FileName $ BS.toShort fName]
    toFileName fDir tp = DirectoryName (BS.toShort $ S8.snoc fDir '/') : tp


instance Eq FileName where
  (==) (FileName f1) (FileName f2) = f1 == f2
  (==) (DirectoryName d1) (DirectoryName d2) = d1 == d2
  (==) f1 f2 = bsFileName f1 == bsFileName f2


instance Hashable FileName where
  hashWithSalt s = hashWithSalt s . bsFileName

instance Ord FileName where
  compare (FileName f1) (FileName f2) = compare f1 f2
  compare f@(FileName fn) d@(DirectoryName dn)
    | f == d = EQ
    | otherwise = compare fn dn
  compare d@(DirectoryName dn) f@(FileName fn)
    | f == d = EQ
    | otherwise = compare dn fn
  compare (DirectoryName d1) (DirectoryName d2) = compare d1 d2


data ShortRef =
  ShortRef !Word64
           !Word64
           !Word32
  deriving (Generic, Eq, Ord)
instance Hashable ShortRef

bsToShortRef ::MonadIO m => ByteString -> m ShortRef
bsToShortRef bs
    | length bs /= 20 = error "Stackage.Package.Git.Types.toShortRef: length is not 20"
    | otherwise = liftIO $ BU.unsafeUseAsCString bs $ \ptr -> ShortRef
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16

toShortRef :: MonadIO m => Ref -> m ShortRef
toShortRef = bsToShortRef . toBinary



getPerson :: GitUser -> IO G.Person
getPerson GitUser {..} = do
  now <- timeCurrent
  return $
    G.Person
    { G.personName = userName
    , G.personEmail = userEmail
    , G.personTime = timeFromElapsed now
    }
