{-# LANGUAGE OverloadedStrings #-}
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

-- |
-- defaultManagerSetting that is  default ManageSetting configuration paramater
-- >>> defaultManagerSetting
-- ManagerSetting {managerSettingPort = 4043 , managerSettingHost = "localhost"}
defaultManagerSetting :: ManagerSetting
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- |
-- newManager is that creategnew manager it manage QUIC runtime
-- for instance, opening/closing UDP socket, some of connection, some of streams,
-- key schedules of each connections.
-- >>> newManager defaultManagerSetting
-- Manager
newManager :: ManagerSetting -> IO Manager
newManager (ManagerSetting port host) = do
    sock <- open host (show port)
    map <- newMVar M.empty
    return $ Manager sock map
      where
        open :: String -> String -> IO S.Socket
        open host port = do
          let hints = S.defaultHints{ S.addrSocketType = S.Datagram  }
          addrs <- S.getAddrInfo (Just hints) (Just port) (Just host)
          let addr = head addrs
          S.socket (S.addrFamily addr) (S.addrSocketType addr) (S.addrProtocol addr)


-- |
-- closeManager is that closing manager
-- that manage all QUIC runtime infomation
-- >>> manager <- newManager defaultManagerSetting
-- >>> closeManager
closeManager :: Manager -> IO ()
closeManager mgr = do
    return undefined

-- |
-- withManager is that run some procedure `IO a` on a top QUIC context.
-- maybe it is not needed in the library, so it will be deprecated
-- when I understand `Manager`.
withManager :: Manager -> IO a -> IO [Frame]
withManager mgr f = undefined
