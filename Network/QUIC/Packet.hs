module Network.QUIC.Packet () where

import           Data.ByteString
import           Network.QUIC.Types


data PlainPacketType = PlainPacketTypeInitial
                | PlainPacketTypeRetry
                | PlainPacketTypeHandshake
                | PlainPacketTypeProtected0RTT
                deriving (Show, Eq)

data ProtectedPacketType = ProtectedPacketTypePADDING
                        | ProtectedPacketTypeRST_STREAM
                        | ProtectedPacketTypeCONNECTION_CLOSE
                        | ProtectedPacketTypeAPPLICATION_CLOSE
                        | ProtectedPacketTypeMAX_DATA
                        | ProtectedPacketTypeMAX_STREAM_DATA
                        | ProtectedPacketTypeMAX_STREAM_ID
                        | ProtectedPacketTypePING
                        | ProtectedPacketTypeBLOCKED
                        | ProtectedPacketTypeSTREAM_BLOCKED
                        | ProtectedPacketTypeSTREAM_ID_BLOCKED
                        | ProtectedPacketTypeNEW_CONNECTION_ID
                        | ProtectedPacketTypeSTOP_SENIDNG
                        | ProtectedPacketTypeACK
                        | ProtectedPacketTypeACK_ECN
                        | ProtectedPacketTypePATH_CHALLENGE
                        | ProtectedPacketTypePATH_RESPONSE
                        | ProtectedPacketTypeNEW_TOKEN
                        | ProtectedPacketTypeSTREAM
                        | ProtectedPacketTypeCRYPTO
                        deriving (Show,Eq)


data LongHeader = LongHeader Version ConnectionId ConnectionId PacketNumber
                deriving (Show, Eq)

data ShortHeader = ShortHeader ConnectionId PacketNumber
                 deriving (Show, Eq)

data InitalPacket = InitalPacket LongHeader Token
                  deriving (Show, Eq)

data RetryPacket = RetryPacket LongHeader ConnectionId Token
                 deriving (Show, Eq)

data HandshakePacket = HandshakePacket LongHeader [Frame]
                     deriving (Show, Eq)

data ProtectedPacket = ProtectedPacket ShortHeader ProtectedPayload
                     deriving (Show, Eq)
