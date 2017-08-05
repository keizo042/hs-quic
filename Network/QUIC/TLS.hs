module Network.QUIC.TLS
  (
  )
  where

import           Data.ByteString    (ByteString)
import           Data.Int
import           Data.Word

import           Network.TLS

import           Network.QUIC.Types
-- TODO: porting from draft code like c++.  It should be replaced more suitable data format.
-- TODO: it demands to adapt TLS. they are requred TLS extension "quic_transport_paramters".

data TransportParameterId = TransParamInitialMaxStreamDataType
                          | TransParamInitialMaxDataType
                          | TransParamInitialMaxStreamIdType
                          | TransParamIdleTimeoutType
                          | TransParamTruncateConnectionIdType
                          | TransParamMaxPacketSizeType
                          | TransParamUnknownType Int16
                          deriving Show


data TransportParameter = TransParamInitialiMaxStreamData Int32
                        | TransParamInitialMaxData Int32
                        | TransParamInitialMaxStreamId StreamId
                        | TransParamInitialIdleTimeout Int16
                        | TransParamTruncateConnectionId ConnectionId
                        | TransParamMaxPacketSize Int16
                        deriving Show

toTransportParameterId :: Word8 -> TransportParameterId
toTransportParameterId 0x00 = TransParamInitialMaxStreamDataType
toTransportParameterId 0x01 = TransParamInitialMaxDataType
toTransportParameterId 0x02 = TransParamInitialMaxStreamIdType
toTransportParameterId 0x03 = TransParamIdleTimeoutType
toTransportParameterId 0x04 = TransParamTruncateConnectionIdType
toTransportParameterId 0x05 = TransParamMaxPacketSizeType

fromTransParameterId :: TransportParameterId -> Word8
fromTransParameterId t = case t of
  TransParamInitialMaxStreamDataType -> undefined
  _                                  -> undefined

data TransportParameters = TransportParametersClientHello QUICVersion QUICVersion [TransportParameter]
                         | TransportParametersEncryptedExtensions [QUICVersion] [TransportParameter]
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
