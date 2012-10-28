{-# LANGUAGE ScopedTypeVariables #-}

module Data.BTree.KVBackend.Util
       ( atomicFileWrite
       , safeWriteFile
       , safeReadFile
       ) where

import Prelude hiding (catch)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Base64 as B64

import Data.Serialize (Serialize, encode, decode)

import System.FilePath ((<.>), (</>))
import System.Directory (renameFile, removeFile)

import Control.Exception

import Data.BTree.UUID


atomicFileWrite path bytes = do
  tmp <- (path <.>) `fmap` show `fmap` uuid
  write tmp
  where
    writeThenMove tmp = do
      B.writeFile tmp bytes
      renameFile tmp path

    write tmp = do
      writeThenMove tmp `finally` (removeFile tmp `catch` \(_::IOError) -> return ())

safeWriteFile path bytes = atomicFileWrite path bytes
safeReadFile  path       = B.readFile path
