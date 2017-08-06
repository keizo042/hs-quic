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

data Typ = ClientHello
         | EncryptedExtension -- be sent with Server Hello
         deriving (Show, Eq)

detectTyp :: IO Typ
detectTyp = undefined

data TLSContext = TLSContext { tlsContextRole :: Typ
                }
                deriving Show

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
