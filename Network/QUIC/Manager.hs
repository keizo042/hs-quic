module Network.QUIC.Manager
  (
  )
  where

import           Control.Concurrent
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar

import qualified Network.Socket          as S

import qualified Data.ByteString         as BS

import           Network.QUIC.Connection
import           Network.QUIC.Types


type Peer = (S.HostAddress, S.PortNumber)

newtype Peers = Peers [(Peers, ConnectionId)]
          deriving Show

emptyPeers :: Peers
emptyPeers = Peers []

-- | ManagerSetting
data ManagerSetting = ManagerSetting { managerSettingPort :: Int
                                     , managerSettingHost :: !String}
                    deriving Show

-- Manager API
-- | defaultManagerSetting that is  default ManageSetting configuration paramater
defaultManagerSetting :: ManagerSetting
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- | Manager is that manage QUIC runtime, socket, mapping connection id to data, etcetc
data Manager = Manager {  managerDataPool    :: MVar DataPool
                        , managerPeers       :: MVar Peers
                        , managerSocket      :: S.Socket
                        , managerThreadId    :: ThreadId
                        , managerDataSender  :: ChannelMap
                        , managerDataReciver :: ChannelMap
                        -- hash from 4 tuple to socket
                       }

openSocket host port = undefined

waitPacket :: S.Socket -> Chan BS.ByteString -> IO ()
waitPacket sock var = undefined

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager (ManagerSetting host port) = do
    sock  <- openSocket host port
    bs    <- newChan
    thid  <- forkIO (waitPacket sock bs)
    pool  <- newMVar emptyDataPool
    peers <- newMVar emptyPeers
    return $  undefined

-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager mgr = do
    return undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager mgr f = undefined
