{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Stackage.Package.Git.Object where

import ClassyPrelude.Conduit
import qualified Codec.Compression.Zlib as Zlib
import Data.ByteArray (convert)
import Crypto.Hash (SHA1(..), Digest)
import Crypto.Hash.Conduit (sinkHash)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Zlib (compress, defaultWindowBits)
import Data.Git.Types as G
import Data.Git.Storage
import Data.Git.Storage.Object
import Data.Git.Ref
import Data.Git.Storage.Loose
import System.Directory
import System.FilePath


import Stackage.Package.Git.Types


-- | Creates a simplified git commit.
makeGitCommit :: Ref -> [Ref] -> GitUser -> ByteString -> IO Commit
makeGitCommit treeRef parents user commitMessage = do
  person <- getPerson user
  return $
    G.Commit
    { G.commitTreeish = treeRef
    , G.commitParents = parents
    , G.commitAuthor = person
    , G.commitCommitter = person
    , G.commitEncoding = Nothing
    , G.commitExtras = []
    , G.commitMessage = commitMessage
    }



-- | Creates a tag of a commit.
makeGitTag :: Ref -> GitUser -> ByteString -> ByteString -> IO Tag
makeGitTag commitRef gitUser tagStr tagMessage = do
  person <- getPerson gitUser
  return $
    G.Tag
    { G.tagRef = commitRef
    , G.tagObjectType = TypeCommit
    , G.tagBlob = tagStr
    , G.tagName = person
    , G.tagS = tagMessage
    }

