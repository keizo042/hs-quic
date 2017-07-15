module Network.QUIC.Manager
  where

import           Network.QUIC.Types
import qualified Network.Socket     as S

data ManagerSetting = ManagerSetting { managerSettingPort :: Int
                                     , managerSettingHost :: !String}
                    deriving Show

-- Manager API
-- default ManageSetting configuration paramater
defaultManagerSetting = ManagerSetting 4043 "localhost"
-- socket, connection stream, key abstraction.
data Manager = Manager { mgrConns :: [Int]
                        ,mgrPeer  :: Int
                        ,mgrSock  :: S.Socket
                        -- hash from 4 tuple to socket
                       }
             deriving Show

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager = return undefined

-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager mgr = undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager = undefined
