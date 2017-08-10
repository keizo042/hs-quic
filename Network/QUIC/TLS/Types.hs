module Network.QUIC.TLS.Types
  (

  Typ(..),
  -- TLSContext
  TLSContext(..),
  Sender(..),
  Reciver(..),

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

data Typ = ClientHello
         | EncryptedExtension -- be sent with Server Hello
         deriving (Show, Eq)

detectTyp :: IO Typ
detectTyp = undefined


-- | Sender from TLS to UDP socket
data Sender = Sender (MVar (StreamId, BS.ByteString))

-- | Reciver from UDP to TLS
data Reciver = Reciver (Chan BS.ByteString) (MVar Bool)

-- | TLSContext
data TLSContext = TLSContext {  tlsContextRole     :: Typ
                              , tlsContextData     :: M.Map StreamId BS.ByteString
                              , tlsContextStreamId :: MVar StreamId
                              , tlsContextSender   :: Sender
                              , tlsContextReciver  :: Reciver
                              }

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
