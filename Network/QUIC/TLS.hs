module Network.QUIC.TLS
  (
    Config(..)
  , tlsContextNew
  )
  where

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception

import           System.Timeout

import qualified Data.ByteString         as BS
import           Data.Int
import           Data.Word

import qualified Network.TLS             as TLS

import           Network.QUIC.Internal
import           Network.QUIC.TLS.Types
import           Network.QUIC.Types

-- entity of transport parameters.
-- TODO: the config will be replaced hash map structure.
-- key: the digit , value: numerical value 0x000 ~ 0xfeff
--
-- because  transport parameters is defined as TLS Extension.
-- TLS Extension have two element, key digit, value octets.
-- it should have similar structure, digit as key, and corresponding value.
data Config = Config { configMaxStreamData :: Int32
            , configMaxData                :: Int32
            , configMaxStreamId            :: StreamId
            , configTimeout                :: Int16
            , configTruncateConnectionId   :: ConnectionId
            , configMaxPacketSize          :: Int16}
            deriving Show

instance TLS.HasBackend Context where
    initializeBackend _ = return ()
    getBackend c = TLS.Backend (return ()) (tlsContextClose ctx) (tlsContextSend ctx) (tlsContextRecv ctx)
      where
        ctx = contextTLSContext c

data QUICException = QUICInternalException
                   deriving Show

instance Exception QUICException


--
--
-- QUIC Context subset as TLS Backend
--
--

-- |  tlsContextNew
tlsContextNew :: Mode -> Int16 -> ConnectionId -> IO Context
tlsContextNew mode i cid = error "initialize TLSContext"

-- | contextClose
tlsContextClose :: TLSContext -> IO ()
tlsContextClose ctx = undefined

-- | tlsContextSend
tlsContextSend :: TLSContext -> BS.ByteString -> IO ()
tlsContextSend ctx bs = send ctx bs
  where
    send :: TLSContext -> BS.ByteString -> IO ()
    send ctx bs = check ctx >>=  \ s -> putMVar sender (s,bs)
      where
        sender = undefined
    check :: TLSContext -> IO StreamId
    check ctx =  undefined

-- | tlsContextRecv
tlsContextRecv :: TLSContext -> Int -> IO BS.ByteString
tlsContextRecv ctx _ = recv bs b
  where
    i = 1000 -- TODO: use timeout value in config
    bs = undefined
    b = undefined
    recv :: (Chan BS.ByteString) -> (MVar Bool) -> IO BS.ByteString
    recv bs b = (timeout i $ takeMVar b) >>= \ flag -> case flag of
      Nothing -> return BS.empty
      _       -> getChanContents bs >>= \ bs' -> return $ BS.concat bs'


