module Network.QUIC.WireEnum ()where

import           Network.QUIC.Packet
import           Network.QUIC.Types


-- | WireEnum encode and decode parameter on the wire format.
class WireEnum a where
    toWireEnum :: Int -> Maybe a
    fromWireEnum :: a -> Int

instance WireEnum Length where
    toWireEnum 0x0 = Just Length1
    toWireEnum 0x1 = Just Length2
    toWireEnum 0x2 = Just Length4
    toWireEnum 0x3 = Just Length8
    toWireEnum _   = Nothing

    fromWireEnum Length1 = 0x00
    fromWireEnum Length2 = 0x01
    fromWireEnum Length4 = 0x02
    fromWireEnum Length8 = 0x03


instance WireEnum PlainPacketType where
    toWireEnum 0x7f = Just PlainPakcetTypeInitial
    toWireEnum 0x7e = Just PlainPakcetTypeRetry
    toWireEnum 0x7d = Just PlainPakcetTypeHandshake
    toWireEnum 0x7c = Just PlainPakcetTypeProtected0RTT
    toWireEnum _    = Nothing

    fromWireEnum PlainPakcetTypeInitial       = 0x7f
    fromWireEnum PlainPakcetTypeRetry         = 0x7e
    fromWireEnum PlainPakcetTypeHandshake     = 0x7d
    fromWireEnum PlainPakcetTypeProtected0RTT = 0x7c

instance WireEnum ProtectedPacketType where
    toWireEnum 0x00 = Just ProtectedPacketTypePADDING
    toWireEnum 0x01 = Just ProtectedPacketTypeRST_STREAM
    toWireEnum 0x02 = Just ProtectedPacketTypeCONNECTION_CLOSE
    toWireEnum 0x03 = Just ProtectedPacketTypeAPPLICATION_CLOSE
    toWireEnum 0x04 = Just ProtectedPacketTypeMAX_DATA
    toWireEnum 0x05 = Just ProtectedPacketTypeMAX_STREAM_DATA
    toWireEnum 0x06 = Just ProtectedPacketTypeMAX_STREAM_ID
    toWireEnum 0x07 = Just ProtectedPacketTypePING
    toWireEnum 0x08 = Just ProtectedPacketTypeBLOCKED
    toWireEnum 0x09 = Just ProtectedPacketTypeSTREAM_BLOCKED
    toWireEnum 0x0a = Just ProtectedPacketTypeSTREAM_ID_BLOCKED
    toWireEnum 0x0b = Just ProtectedPacketTypeNEW_CONNECTION_ID
    toWireEnum 0x0c = Just ProtectedPacketTypeSTOP_SENIDNG
    toWireEnum 0x0d = Just ProtectedPacketTypeACK
    toWireEnum 0x20 = Just ProtectedPacketTypeACK_ECN
    toWireEnum 0x0e = Just ProtectedPacketTypePATH_CHALLENGE
    toWireEnum 0x0f = Just ProtectedPacketTypePATH_RESPONSE
    toWireEnum 0x19 = Just ProtectedPacketTypeNEW_TOKEN
    toWireEnum 0x18 = Just ProtectedPacketTypeCRYPTO
    toWireEnum v = if (0x10 <= v && v <= 0x17) then Just ProtectedPacketTypeSTREAM else Nothing

    fromWireEnum ProtectedPacketTypePADDING           = 0x00
    fromWireEnum ProtectedPacketTypeRST_STREAM        = 0x01
    fromWireEnum ProtectedPacketTypeCONNECTION_CLOSE  = 0x02
    fromWireEnum ProtectedPacketTypeAPPLICATION_CLOSE = 0x03
    fromWireEnum ProtectedPacketTypeMAX_DATA          = 0x04
    fromWireEnum ProtectedPacketTypeMAX_STREAM_DATA   = 0x05
    fromWireEnum ProtectedPacketTypeMAX_STREAM_ID     = 0x06
    fromWireEnum ProtectedPacketTypePING              = 0x07
    fromWireEnum ProtectedPacketTypeBLOCKED           = 0x08
    fromWireEnum ProtectedPacketTypeSTREAM_BLOCKED    = 0x09
    fromWireEnum ProtectedPacketTypeSTREAM_ID_BLOCKED = 0x0a
    fromWireEnum ProtectedPacketTypeNEW_CONNECTION_ID = 0x0b
    fromWireEnum ProtectedPacketTypeSTOP_SENIDNG      = 0x0c
    fromWireEnum ProtectedPacketTypeACK               = 0x0d
    fromWireEnum ProtectedPacketTypeACK_ECN           = 0x20
    fromWireEnum ProtectedPacketTypePATH_CHALLENGE    = 0x0e
    fromWireEnum ProtectedPacketTypePATH_RESPONSE     = 0x0f
    fromWireEnum ProtectedPacketTypeNEW_TOKEN         = 0x19
    fromWireEnum ProtectedPacketTypeCRYPTO            = 0x18
    fromWireEnum ProtectedPacketTypeSTREAM            = 0x10 -- build with OFF(0x04), LEN(0x02), FIN(0x01) bit.
