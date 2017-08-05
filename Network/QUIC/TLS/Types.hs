module Network.QUIC.TLS.Types
  (
  Typ(..),
  TLSContext(..),
  TransportParameterId(..),
  TransportParameter(..),
  TransportParameters(..)
  )
  where

import           Data.Int
import           Network.QUIC.Types

data Typ = ClientHello | EncryptedExtension
         deriving (Show, Eq)
data TLSContext = TLSContext { tlsContextRole :: Typ
                }
                deriving Show

data TransportParameterId = TransParamInitialMaxStreamDataType
                          | TransParamInitialMaxDataType
                          | TransParamInitialMaxStreamIdType
                          | TransParamIdleTimeoutType
                          | TransParamTruncateConnectionIdType
                          | TransParamMaxPacketSizeType
                          | TransParamUnknownType Int16
                          deriving Show


data TransportParameter = TransParamInitialMaxStreamData Int32
                        | TransParamInitialMaxData Int32
                        | TransParamInitialMaxStreamId StreamId
                        | TransParamIdleTimeout Int16
                        | TransParamTruncateConnectionId ConnectionId
                        | TransParamMaxPacketSize Int16
                        deriving Show

data TransportParameters = TransportParametersClientHello QUICVersion QUICVersion [TransportParameter]
                         | TransportParametersEncryptedExtensions [QUICVersion] [TransportParameter]
                         deriving Show
