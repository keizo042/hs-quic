module Network.QUIC.Manager
  (
    newManager
  , closeManager
  , withManager
  , defaultManagerSetting
  )
  where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar

import qualified Network.Socket          as S

import qualified Data.ByteString         as BS
import qualified Data.Map                as M

import           Network.QUIC.Connection
import           Network.QUIC.Types

-- Manager API
-- | defaultManagerSetting that is  default ManageSetting configuration paramater
defaultManagerSetting :: ManagerSetting
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager (ManagerSetting host port) = do
    sock <- open host port
    map <- newMVar M.empty
    return $ Manager sock map
      where
        open :: ByteString -> Int -> IO Socket
        open host port = do
          let hints = S.defaultHints{ S.addrSocketType = S.Datagram  }
          addrs <- S.getAddrInfo (Just hints) (Just host) (Just port)
          let addr = head addrs
          S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)


-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager mgr = do
    return undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager mgr f = undefined
