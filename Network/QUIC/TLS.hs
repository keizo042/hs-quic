module Network.QUIC.TLS
  (
    Config(..)
  , contextNew
  , contextSend
  , contextRecv
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
data Config = Config { configMaxStreamData :: Int32
            , configMaxData                :: Int32
            , configMaxStreamId            :: StreamId
            , configTimeout                :: Int16
            , configTruncateConnectionId   :: ConnectionId
            , configMaxPacketSize          :: Int16}
            deriving Show

instance TLS.HasBackend Context where
    initializeBackend _ = return ()
    getBackend c = TLS.Backend (\ _ -> return ()) (contextClose ctx) (contextSend ctx) (contextRecv ctx)
      where
        ctx = new c
        new :: Context -> TLSContext
        new = undefined

data QUICException = QUICInternalException
                   deriving Show

instance Exception QUICException

-- |  contextNew
contextNew :: Mode -> Int16 -> ConnectionId -> IO Context
contextNew mode i cid = error "initialize TLSContext"

-- | contextSend
contextSend :: TLSContext -> BS.ByteString -> IO ()
contextSend ctx bs = send ctx bs
  where
    send :: TLSContext -> BS.ByteString -> IO ()
    send ctx bs = check ctx >>=  \ s -> putMVar sender (s,bs)
      where
        (Sender sender) = tlsContextSender ctx

    check :: TLSContext -> IO StreamId
    check ctx =  tryReadMVar (tlsContextStreamId ctx) >>= \ ident -> case ident of
      (Just v) -> modifyMVar (tlsContextStreamId ctx) (\ i -> return (i + 2, i + 2))
      Nothing  -> throwIO QUICInternalException -- TODO: consider exception type

-- | ContextRecv
contextRecv :: TLSContext -> Int -> IO BS.ByteString
contextRecv ctx _ = recv bs b
  where
    i = 1000 -- TODO: use timeout value in config
    (Reciver bs b)  = tlsContextReciver ctx
    recv :: (Chan BS.ByteString) -> (MVar Bool) -> IO BS.ByteString
    recv bs b = (timeout i $ takeMVar b) >>= \ flag -> case flag of
      Nothing -> return BS.empty
      _       -> getChanContents bs >>= \ bs' -> return $ BS.concat bs'


