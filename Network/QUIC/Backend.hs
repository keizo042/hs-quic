module Network.QUIC.Backend
  (
    Backend
  , BackendParams
  , defaultParams
  , newBackend
  , newBackendWith
  ) where

import           Network.Socket

-- | Transport Backend.
-- | it maintains
-- | - socket
-- | - corresponding connection id to connection context.
data Backend = Backend { backendSocket :: Socket
             }

data BackendParams

defaultParams :: BackendParams
defaultParams = undefined

newBackend :: HostName -> PortNumber -> IO Backend
newBackend = newBackendWith defaultParams

newBackendWith :: BackendParams -> HostName -> PortNumber -> IO Backend
newBackendWith params hostname port = undefined
