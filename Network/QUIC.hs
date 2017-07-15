module Network.QUIC
  where
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int
import qualified Data.Map.Strict       as M
import           Data.Maybe            (Maybe)
import qualified Data.Maybe            as Maybe
import qualified Data.Time.Clock       as Clock
import           Data.Word             (Word8)
import qualified Network.QUIC.Internal as I
import           Network.QUIC.Time
import           Network.QUIC.TLS
import           Network.QUIC.Types
import qualified Network.Socket        as S
import qualified Network.TLS           as TLS

errorCodeToInt :: ErrorCode -> Int
errorCodeToInt (ApplicationErrorCode i) = i
errorCodeToInt (HostLocalErrorCode i)   = i
errorCodeToInt (QUICErrorCode err)      = quicErrorToInt err
errorCodeToInt (CrypotograhicError i)   = i

intToErrorCode :: Int -> ErrorCode
intToErrorCode i
 | (0x00000000 <= i && i <= 0x3FFFFFFF) = ApplicationErrorCode i
 | (0x40000000 <= i && i <= 0x7FFFFFFF) = HostLocalErrorCode i
 | (0x80000000 <= i && i <= 0xBFFFFFFF) = QUICErrorCode (intToQUICError i)
 | (0xC0000000 <= i && i <= 0xFFFFFFFF) = CrypotograhicError i

quicErrorToInt :: QUICError -> Int
quicErrorToInt QUICInternalError              = 0x80000001
quicErrorToInt QUICStreamDataAfterTermination = 0x80000002
quicErrorToInt QUICInvalidStreamData          =  undefined
quicErrorToInt _                              =  undefined

intToQUICError :: Int -> QUICError
intToQUICError i = f (i - 0x80000000)
  where
    f :: Int -> QUICError
    f 0x01 = QUICInternalError
    f 0x02 = QUICStreamDataAfterTermination
    f 0x03 = QUICInvalidPacketHeader
    f 0x04 = QUICInvalidFrameData
    f 0x05 = QUICMultipleTerminationOffsets
    f 0x06 = QUICStreamCancelled
    f 0x07 = QUICClosedCriticalStream
    f 0x30 = QUICMissingPayload
    f 0x2e = QUICInvalidStreamData
    f 0x3d = QUICUnencryptedStreamData
    f 0x59 = QUICMaybeCorruptedMemory
    f 0x06 = QUICInvalidRstStreamData
    f 0x07 = QUICInvalidConnectionCloseData
    f 0x08 = QUICInvalidGoawayData
    f 0x39 = QUICInvalidWindowUpdateData
    f 0x3a = QUICInvalidBlockedData
    f 0x4e = QUICInvalidPathCloseData
    f 0x09 = QUICInvalidAckData
    f 0x0a = QUICInvalidVersionNegotiationPacket
    f 0x0b = QUICInvalidPublicReset
    f 0x0c = QUICDecryptionFailure
    f 0x0d = QUICEncryptionFailure
    f 0x0e = QUICPacketTooLarge
    f 0x10 = QUICPeerGoingAway
    f 0x11 = QUICInvalidStreamId
    f 0x31 = QUICInvalidPriority
    f 0x12 = QUICTooManyOpenStreams
    f 0x4c = QUICTooManyAvailableStreams
    f 0x13 = QUICPublicReset
    f 0x14 = QUICInvalidVersion
    f 0x16 = QUICInvalidHeaderId
    f 0x17 = QUICInvalidNegotiationValue
    f 0x18 = QUICDecompressionFailure
    f 0x19 = QUICNetworkIdleTimeout
    f 0x43 = QUICHandshakeTimeout
    f 0x2a = QUICErrorMigratingAddress
    f 0x56 = QUICErrorMigratingPort
    f 0x32 = QUICEmptyStreamFrameNoFin
    f 0x3b = QUICFlowControlRecievedTooMuchData
    f 0x3f = QUICFlowControlSentTooMuchData
    f 0x40 = QUICFlowControlInvalidWindow
    f 0x3e = QUICConnectionIpPooled
    f 0x44 = QUICTooManyOutstandingSentPackets
    f 0x45 = QUICTooManyOutstandingRecievedPackets
    f 0x46 = QUICConnectionCancelled
    f 0x47 = QUICBadPacketLossRate
    f 0x49 = QUICPublicResetPostHandshake
    f 0x4a = QUICTimeoutsWithOpenStreams
    f 0x55 = QUICTooManyRTOs
    f 0x2c = QUICEncryptionLevelIncorrect
    f 0x37 = QUICVersionNegotiationMissmatch
    f 0x50 = QUICIpAddressChanged
    f 0x51 = QUICAddressValidationFailure
    f 0x5d = QUICTooManyFrameGaps
    f 0x60 = QUICTooManySessionsOnServers
    f _    = error "there are some conflict erro code"

upperLimitMaxPacketSize :: Int -> Bool
upperLimitMaxPacketSize i = i < 65527

lowerLimitMaxPacketSize :: Int -> Bool
lowerLimitMaxPacketSize i = i > 1252

-- the number that library support protocol version.
supportedVersion :: [Int32]
supportedVersion = [0x0d,
                    0x0e
                   ]

-- latest number that the libary support
defaultVersion :: Int32
defaultVersion = 0x0e

-- QUIC Version  Negtiaton

isVaildVersion :: QUICVersion -> Bool
isVaildVersion = undefined

isSupportedversion :: QUICVersion -> Bool
isSupportedversion = undefined

supportedVersions :: [QUICVersion] -> [QUICVersion]
supportedVersions = undefined

waitData :: StreamId -> ByteString
waitData = undefined

-- testing
testDataMap = undefined
testStreamData = undefined
