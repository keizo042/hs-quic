module Network.QUIC.Manager
  (
  )
  where

import           Control.Concurrent
import           Control.Concurrent.MVar

import qualified Network.Socket          as S

import           Network.QUIC.Connection
import           Network.QUIC.Types


type Peer = (HostAddress, PortNumber)

data Peers = Peers [(Peers, ConnectionId)]
          deriving Show

-- | ManagerSetting
data ManagerSetting = ManagerSetting { managerSettingPort :: Int
                                     , managerSettingHost :: !String}
                    deriving Show

-- Manager API
-- | defaultManagerSetting that is  default ManageSetting configuration paramater
defaultManagerSetting :: ManagerSetting
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- | Manager is that manage QUIC runtime, socket, mapping connection id to data, etcetc
data Manager = Manager {  managerDataPool     :: MVar DataPool
                        , managerPeers        :: MVar Peers
                        , managerSocket       :: S.Socket
                        , managerRecvThreadId :: ThreadId
                        -- hash from 4 tuple to socket
                       }
             deriving Show


openSocket host port = undefined

waitPacket :: Socket -> MVar ByteString -> IO ()
waitPacket sock var = undefined

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager setting = do
    sock <- openSocket
    thid <- forkIO undefined
    pool <- newMVar emptyDataPool
    peers <- newMVar emptyPeers
    return $  Manager emptyDataPool peers sock thid

-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager (Manager pool peers sock thread)= do
    return undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager (Manager pool perrs sock thread) f = undefined
