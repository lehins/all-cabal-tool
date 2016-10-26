{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Stackage.Package.Git.WorkTree where

import ClassyPrelude.Conduit
import Control.Monad
import Data.Bits
import qualified Data.ByteString as S
import qualified Data.ByteString.Short as BS
import qualified Data.ByteString.Char8 as S8
import Data.Byteable
import Data.Git hiding (WorkTree)
import Data.Git.Ref
import Data.Git.Storage
import Data.Git.Storage.Object
import qualified Data.HashTable.IO as H
import qualified Data.Map.Strict as Map

import Stackage.Package.Git.Types
import Stackage.Package.Git.Object





flushTree :: GitRepository -> IO Ref
flushTree GitRepository {repoInstance = GitInstance {..}} = makeTree gitWorkTree
  where
    makeTree workTree = do
      m <- H.foldM addEntry mempty' workTree
      let obj = ObjTree $ Tree $ map snd $ Map.toAscList m
          objBS = objectWrite obj
          ref = objectHash TypeTree (fromIntegral $ length objBS) objBS
      sRef <- toShortRef ref
      unlessM (doesRefExist gitObjects sRef) $
        do ref' <- setObject gitRepo obj
           unless (ref == ref') $ error "makeTree: mismatch"
           H.insert gitObjects sRef ()
      return ref
      where
        mempty' :: Map.Map FileName (ModePerm, EntName, Ref)
        mempty' = mempty
        addEntry m (name, entry) = do
          (mode, ref) <-
            case entry of
              TEDir subTree -> do
                ref <- makeTree subTree
                return (0o040000, ref)
              TEFile ref -> return (0o100644, ref)
          return $!
            insertMap name (ModePerm mode, entName $ bsFileName name, ref) m


insertRef :: GitRepository -> TreePath -> Ref -> IO ()
insertRef GitRepository {repoInstance = GitInstance {..}} tp ref = loop tp gitWorkTree
  where
    loop [] _ = error "insertRef: Filepath should not be empty"
    loop [fName@(FileName _)] tree =
      H.insert tree fName (TEFile ref)
    loop (dirName:rest) tree = do
      mres <- H.lookup tree dirName
      tree' <-
        case mres of
          Nothing -> do
            tree' <- H.new
            H.insert tree dirName $ TEDir tree'
            return tree'
          Just (TEDir tree') -> return tree'
          Just (TEFile _) -> error $ "Unexpected file"
      loop rest tree'

persistBlob :: GitRepository -> LByteString -> IO Ref
persistBlob GitRepository {repoInstance = GitInstance {..}} lbs = do
  let ref = objectHash TypeBlob (fromIntegral $ length lbs) lbs
  sRef <- toShortRef ref
  unlessM (doesRefExist gitObjects sRef) $
    do ref' <- setObject gitRepo (ObjBlob (Blob lbs))
       unless (ref == ref') $ error "getBlobRef: mismatch"
       H.insert gitObjects sRef ()
  return ref


doesRefExist :: MonadIO m => ObjectsTable -> ShortRef -> m Bool
doesRefExist objectsTable short = liftIO $ isJust <$> H.lookup objectsTable short


lookupFileRef :: WorkTree -> TreePath -> IO (Maybe Ref)
lookupFileRef tree [fName@FileName {}] = do
  mres <- H.lookup tree fName
  return $
    case mres of
      Just (TEFile ref) -> Just ref
      _ -> Nothing
lookupFileRef tree (dirName:rest) = do
  msubTree <- H.lookup tree dirName
  case msubTree of
    Just (TEDir subTree) -> lookupFileRef subTree rest
    _ -> return Nothing
