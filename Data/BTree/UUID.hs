{-# LANGUAGE DeriveDataTypeable
           , GeneralizedNewtypeDeriving
  #-}

module Data.BTree.UUID
       ( UUID
       , uuid
       )
       where


import Data.Word
import Text.Printf
import Control.Concurrent
import Data.Typeable

import qualified Data.Char as C

import Data.Serialize

import System.IO.Unsafe
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Base64     as B64

newtype UUID = UUID { unUUID :: B.ByteString }
             deriving (Ord, Eq)


instance Serialize UUID where
  put = put . unUUID
  get = UUID `fmap` get



instance Show UUID where
  show (UUID bs) = B.unpack $ B64.encode bs


uuids :: MVar Int
uuids = unsafePerformIO $ newMVar 1
{-# NOINLINE uuids #-}


uuid :: IO UUID
uuid = (UUID . encode) `fmap`
       (modifyMVar uuids $ \n -> return (n+1, n))


