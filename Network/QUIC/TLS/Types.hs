module Network.QUIC.TLS.Types
  (

  -- tls extension: quic_transport_parameter
  TransportParameterId(..),
  TransportParameter(..),
  TransportParameters(..)
  )
  where

import qualified Data.ByteString         as BS
import           Data.Int
import qualified Data.Map.Strict         as M

import           Network.QUIC.Types

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar

-- | TransportParameterId that indicates transport parameter type.
data TransportParameterId = TransParamInitialMaxStreamDataType
                          | TransParamInitialMaxDataType
                          | TransParamInitialMaxStreamIdType
                          | TransParamIdleTimeoutType
                          | TransParamTruncateConnectionIdType
                          | TransParamMaxPacketSizeType
                          | TransParamUnknownType Int16
                          deriving (Show, Eq)

-- | TransportParameter is that indicates parameter.
data TransportParameter = TransParamInitialMaxStreamData Int32
                        | TransParamInitialMaxData Int32
                        | TransParamInitialMaxStreamId StreamId
                        | TransParamIdleTimeout Int16
                        | TransParamTruncateConnectionId ConnectionId
                        | TransParamMaxPacketSize Int16
                        deriving (Show, Eq)

data TransportParameters = TransportParametersClientHello QUICVersion QUICVersion [TransportParameter]
                         | TransportParametersEncryptedExtensions [QUICVersion] [TransportParameter]
                         deriving (Show, Eq)
