module Network.QUIC.Types
  where

import qualified Data.Binary        as B
import qualified Data.Binary.Get    as Get
import qualified Data.Binary.Put    as Put
import           Data.Bits
import           Data.ByteString    (ByteString)
import           Data.Default.Class
import           Data.Int
import qualified Data.Time.Clock    as Clock

-- Packet Context, it is indicated by header mainly.
data PacketContext = PacketContext { contextPacketNumberSize :: Int
                                   , contextStreamSize       :: StreamSize
                                   , contextOffsetSize       :: OffsetSize}
             deriving (Show, Eq)

data Header = LongHeader LongHeaderType ConnectionId PacketNumber QUICVersion
            | ShortHeader (Maybe ConnectionId) PacketNumber
            deriving Show

data HeaderType = LongHeaderType | ShortHeaderType
                deriving (Show, Eq)

data LongHeaderType = VersionNegotiationType
                    | ClientInitialType
                    | ServerStatelessRetryType
                    | ServerCleartextType
                    | ClientCleartextType
                    | ZeroRTTProtectedType
                    | OneRTTProtectedType
                    | OneRTTProtectedKeyPhaseZeroType
                    | OneRTTProctectedKeyPhaseOneType
                    | PublicResetType
                    deriving (Show, Eq)

data LongHeaderPacket = VersionNegotiation  QUICVersion [QUICVersion]
                | ClientInitial
                | ServerStatelessRetry
                | ServerCleartext
                | ClientCleartext
                | ZeroRTTProtected
                | OneRTTProtectedKeyPhaseZero
                | OneRTTProtectedKeyPhaseOne
                | PublicReset
                deriving (Show, Eq)

data StreamSize = Stream1Byte | Stream2Byte | Stream3Byte | Stream4Byte
                deriving (Show, Eq)

data OffsetSize = NoExistOffset | Offset2Byte | Offset4Byte | Offset8Byte
                deriving (Show, Eq)

data LAckSize = LAck1Byte | LAck2Byte | LAck4Byte | LAck8Byte
                deriving (Show, Eq)

data AckBlockLengthSize = AckBlock1Byte | AckBlock2Byte | AckBlock4Byte | AckBlock8Byte
                deriving (Show, Eq)

data FrameType = StreamType Bool StreamSize OffsetSize Bool
               | AckType Bool LAckSize AckBlockLengthSize
               | MaxDataType
               | MaxStreamDataType
               | MaxStreamIdType
               | BlockedType
               | StreamBlockedType
               | StreamIdNeededType
               | RstStreamType
               | PaddingType
               | PingType
               | NewConnectionType
               | ConnectionCloseType
               | GoawayType
               deriving Show

type StreamId = Int

type Offset = Integer

data QUICTime = QUICTime
             deriving Show

data AckBlock = AckBlock [PacketNumber]
              deriving Show

data AckTimeStamp = AckTimeStamp [(Int, QUICTime)]
                  deriving Show

data Frame = Stream !StreamId !Offset !ByteString
           | Ack !(Maybe Int) !Int !PacketNumber !QUICTime !QUICTime !AckBlock !AckTimeStamp
           | MaxData !Int64
           | MaxStreamData !StreamId !Int
           | MaxStreamId !StreamId
           | Blocked
           | StreamBlocked !StreamId
           | StreamIdNeeded
           | RstStream !StreamId !ErrorCode !Offset
           | Padding
           | Ping
           | NewConnectionId !Int !ConnectionId
           | ConnectionClose !ErrorCode !ByteString
           | Goaway !StreamId !StreamId
           deriving Show


type PacketNumber = Integer

type ConnectionId = Integer

type QUICVersion = Int32

data Packet = LongPacket Header LongHeaderPacket Payload
            | ShortPacket Header Payload
            deriving Show
type Payload = [Frame]

data ErrorCode = ApplicationErrorCode Int
               | HostLocalErrorCode Int
               | QUICErrorCode QUICError
               | CrypotograhicError Int
               deriving Show

data QUICError = QUICInternalError
               | QUICStreamDataAfterTermination
               | QUICInvalidPacketHeader
               | QUICInvalidFrameData
               | QUICMultipleTerminationOffsets
               | QUICStreamCancelled
               | QUICClosedCriticalStream
               | QUICMissingPayload
               | QUICInvalidStreamData
               | QUICUnencryptedStreamData
               | QUICMaybeCorruptedMemory
               | QUICInvalidRstStreamData
               | QUICInvalidConnectionCloseData
               | QUICInvalidGoawayData
               | QUICInvalidWindowUpdateData
               | QUICInvalidBlockedData
               | QUICInvalidPathCloseData
               | QUICInvalidAckData
               | QUICInvalidVersionNegotiationPacket
               | QUICInvalidPublicReset
               | QUICDecryptionFailure
               | QUICEncryptionFailure
               | QUICPacketTooLarge
               | QUICPeerGoingAway
               | QUICInvalidStreamId
               | QUICInvalidPriority
               | QUICTooManyOpenStreams
               | QUICTooManyAvailableStreams
               | QUICPublicReset
               | QUICInvalidVersion
               | QUICInvalidHeaderId
               | QUICInvalidNegotiationValue
               | QUICDecompressionFailure
               | QUICNetworkIdleTimeout
               | QUICHandshakeTimeout
               | QUICErrorMigratingAddress
               | QUICErrorMigratingPort
               | QUICEmptyStreamFrameNoFin
               | QUICFlowControlRecievedTooMuchData
               | QUICFlowControlSentTooMuchData
               | QUICFlowControlInvalidWindow
               | QUICConnectionIpPooled
               | QUICTooManyOutstandingSentPackets
               | QUICTooManyOutstandingRecievedPackets
               | QUICConnectionCancelled
               | QUICBadPacketLossRate
               | QUICPublicResetPostHandshake
               | QUICTimeoutsWithOpenStreams
               | QUICTooManyRTOs
               | QUICEncryptionLevelIncorrect
               | QUICVersionNegotiationMissmatch
               | QUICIpAddressChanged
               | QUICAddressValidationFailure
               | QUICTooManyFrameGaps
               | QUICTooManySessionsOnServers
               | QUICUnkownErrorCode Int
               deriving (Show, Eq)

-- TODO: error code decoder
getErrorCode :: Get.Get ErrorCode
getErrorCode = undefined

type QUICResult a = Either QUICError a

data Context = Context { ctxMode    :: Mode
                       , ctxVersion :: QUICVersion }
             deriving Show


data Mode = Client | Server
          deriving Show

