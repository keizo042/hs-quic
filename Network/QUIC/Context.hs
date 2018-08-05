module Network.QUIC.Context
  ( Context
  ) where

import           Control.Concurrent.MVar
import           Network.QUIC.Backend
import           Network.QUIC.Types

-- | `Context` is a connection context.
data Context = Context {
               ctxConnectionState :: ConnectionState
             , ctxKeyState        :: KeyState
             }

contextNew :: Backend -> IO Context
contextNew = undefined
-- TODO:
-- contextNew create new connection context.

contextClose :: Context -> IO ()
contextClose = undefined
-- TODO:
-- contextClose close all state and terminate peer ConnectionState.
