{-# LANGUAGE ScopedTypeVariables #-}

module Test.Util
       ( byteStringToFileName
       , fileNameToByteString
       , clearDirectory
       , safeForkIO
       , safeKill
       , decode'
       )
       where

import Prelude hiding (catch)

import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Base64 as B64
import System.Directory
import Control.Monad

import Data.Serialize (encode, decode)

import System.FilePath ((<.>))
import System.Directory (renameFile)

import Control.Concurrent
import Control.Exception


byteStringToFileName k = map fixChars $ B.unpack $ B64.encode k
  where
    fixChars '/' = '-'
    fixChars c   = c

fileNameToByteString f = unRight $ B64.decode $ B.pack $ map unfixChars f
  where
    unRight (Right x) = x
    unfixChars '-' = '/'
    unfixChars c   = c

clearDirectory path = do
  metaExists <- doesDirectoryExist path
  when metaExists $
    removeDirectoryRecursive path
  createDirectoryIfMissing True path


safeForkIO f = do
  mv  <- newEmptyMVar
  pid <- forkIO $ f mv
  putMVar mv pid
  return mv


safeKill m = do
  withMVar m killThread


decode' errmsg a =
  either (\msg -> error $ errmsg ++ ": " ++ msg) id $ decode a