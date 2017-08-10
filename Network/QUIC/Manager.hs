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

import           Network.QUIC.Connection
import           Network.QUIC.Types

-- Manager API
-- | defaultManagerSetting that is  default ManageSetting configuration paramater
defaultManagerSetting :: ManagerSetting
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager (ManagerSetting host port) = undefined

-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager mgr = do
    return undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager mgr f = undefined
