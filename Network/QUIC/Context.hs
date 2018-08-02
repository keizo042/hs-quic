module Network.QUIC.Context () where

-- | `Context` is a connection context.
data Context = Context { cxtConnectionId :: ConnectionId
             }

