module Network.QUIC.TLS
  where

import           Data.ByteString    (ByteString)
import           Network.QUIC.Types
-- TODO: porting from draft code like c++.  It should be replaced more suitable data format.
-- TODO: it demands to adapt TLS. they are requred TLS extension "quic_transport_paramters".

data TransportParameterId = TransParamInitialMaxStreamData
                          | TransParamInitialMaxData
                          | TransParamInitialMaxStreamId
                          | TransParamIdleTimeout
                          | TransParamTruncateConnectionId
                          | TransParamMaxPacketSize
                          | TransParamUnknown Int
                          deriving Show

data TransportParameter = TransportParameter {
                          transParamId    :: TransportParameterId
                        , transParamValue :: ByteString

                        }
                        deriving Show

data TransportParameters = TransportParametersClientHello QUICVersion QUICVersion [TransportParameter]
                         | TransportPArametersEncryptedExtensions [QUICVersion] [TransportParameter]
                         deriving Show

-- entity of transport parameters.
-- TODO: the config will be replaced hash map structure.
-- key: the digit , value: numerical value 0x000 ~ 0xfeff
data Config = Config { configMaxStreamData :: Int
            , configMaxData                :: Int
            , configMaxStreamId            :: StreamId
            , configTimeout                :: QUICTime
            , configTruncateConnectionId   :: ConnectionId
            , configMaxPacketSize          :: Int}
            deriving Show

-- TODO: having our own the handshake procedure is bad practice.
-- We should carry TLS handshake from Network.TLS (tls package)
-- but api is nothing in tls package. So we have implementation for now.
-- it will be replaced.

sendHandshake1RTT = undefined
  where
  -- send Client Hello with 1RTT
  sendClientHello = undefined

  -- recv Server Hello with 1RTT
  recvServerHello = undefined


recvHandshake = undefined
  where
  -- recv Client Hello with 1RTT
  recvClinetHello = undefined

  sendServerHello = undefined
--
