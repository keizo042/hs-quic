module Network.QUIC.Packet
  (
    Packet(..)
  , recvPacket
  , sendPacket

  ,
    recvPacket
  ,
  ) where

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


data LongHeader = LongHeader Version (Maybe ConnectionID) (Maybe ConnectionID) PacketNumber
                deriving (Show, Eq)

data ShortHeader = ShortHeader ConnectionId PacketNumber
                 deriving (Show, Eq)


data Packet = PacketInitial InitialPacket
            | PacketRetry RetryPacket
            | PacketHandshake HandshakePacket
            | PacketVersionNegotiation VersionNegotiationPacket
            | PacketProcteded ProtectedPacket
            deriving (Show, Eq)

data InitialPacket = InitialPacket LongHeader Token
                  deriving (Show, Eq)

data VersionNegotiationPacket = VersionNegotiationPacket Version (Maybe ConnectionID) (Maybe ConnectionID) Version [Version]

data RetryPacket = RetryPacket LongHeader ConnectionId Token
                 deriving (Show, Eq)

data HandshakePacket = HandshakePacket LongHeader [Frame]
                     deriving (Show, Eq)

data ProtectedPacket = ProtectedPacket ShortHeader ProtectedPayload
                     deriving (Show, Eq)


sendPacket :: Context -> Packet -> IO ()
sendPacket ctx pkt = undefind
-- TODO: encrypt with prefer TLS ecnryption level in Context send sockaddr
-- corresponding ConnectionID.


recvPacket :: Context -> IO Packet
recvPacket ctx = undefined
-- TODO: decrypt with keys in Context and put haskell ADT.
-- raise error when
-- * invalid packet form.
-- * invalid encryption.
--
-- not raise error at here
-- * invalid packet on the processing stage.
