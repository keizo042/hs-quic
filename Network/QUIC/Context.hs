module Network.QUIC.Context () where

import           Network.QUIC.Backend
import           Network.QUIC.Types

-- | `Context` is a connection context.
data Context = Context { cxtConnectionId :: ConnectionId
             }

contextNew :: Backend -> IO Context
contextNew = undefined

contextClose :: Context -> IO ()
contextClose = undefined
