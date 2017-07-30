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


-- | QUICResult is result type in the QUIC protocol context.
type QUICResult a = Either QUICError a


-- | Context is type that indicates the library main info context.
data Context = Context { ctxMode    :: Mode
                       , ctxVersion :: QUICVersion }
             deriving Show



-- | LongHeaderContext
data LongPacketContext = LongPacketContext { longPacketContextLongHeaderType :: LongHeaderType
                                           , longPacketContextKeyPhase :: Bool }
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

data LongPacketPayload = VersionNegotiation  QUICVersion [QUICVersion]
                | ClientInitial
                | ServerStatelessRetry
                | ServerCleartext
                | ClientCleartext
                | ZeroRTTProtected
                | OneRTTProtectedKeyPhaseZero
                | OneRTTProtectedKeyPhaseOne
                | PublicReset
                deriving (Show, Eq)

-- | ShortPacketPayload
type ShortPacketPayload = [Frame]

-- | Frame
-- TODO: note commnets.
data Frame = Stream !StreamId !Offset !ByteString
           | Ack !PacketNumber !QUICTime !AckBlock !AckTimeStamp
           | MaxData !Int64
           | MaxStreamData !StreamId !Int
           | MaxStreamId !StreamId
           | Blocked
           | StreamBlocked !StreamId
           | StreamIdNeeded
           | RstStream !StreamId !ErrorCode !Offset
           | Padding
           | Ping
           | NewConnectionId !Int !ConnectionId -- Sequence ConnectionId
           | ConnectionClose !ErrorCode !ByteString -- ErrorCode ErrorMessage
           | Goaway !StreamId !StreamId
           deriving Show


data PacketNumberSize = PacketNumber1Byte | PacketNumber2Byte | PacketNumber4Byte

data DecodeContext = DecodeContext { decodeContextStreamSize :: StreamSize
                                   , decodeContextOffsetSize :: OffsetSize
                                   , decodeContextPacketNumberSize :: PacketNumberSize
                                   , decodeContextLongPacketContext :: Maybe LongPacketContext}

data EncodeContext = EncodeContext { encodeContextPacketNumberSize :: PacketNumberSize
                                   , encodeContextStreamSize :: StreamSize
                                   , encodeContextOffsetSize :: OffsetSize  }

-- | Internal use
data StreamSize = Stream1Byte | Stream2Byte | Stream3Byte | Stream4Byte
                deriving (Show, Eq)

-- | Internal use
data OffsetSize = NoExistOffset | Offset2Byte | Offset4Byte | Offset8Byte
                deriving (Show, Eq)

-- | Internal use
data LAckSize = LAck1Byte | LAck2Byte | LAck4Byte | LAck8Byte
                deriving (Show, Eq)

-- | Internal use
data AckBlockLengthSize = AckBlock1Byte | AckBlock2Byte | AckBlock4Byte | AckBlock8Byte
                deriving (Show, Eq)

-- | FrameType is for internal use.
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

-- | StreamId is type that indicate identify in stream.
type StreamId = Int

-- | Offset is type that indicate offset from Head of Data in Stream Frame
type Offset = Integer

-- | QUICTime
-- TODO: See [draft-ietf-quic-transport-04] Section 8.2.2.1 "Time Format"
-- We Must implement IEEE74 like time format and the utility.
-- 16bit unsinged float.
-- mantissa 11 bit
-- exponent 5 bit
-- time in microseconds
data QUICTime = QUICTime Int16 -- QUICTime Int Int
             deriving Show

-- | AckBlock is Blocks that is recived  in Ack Frame.
data AckBlock = AckBlock [PacketNumber]
              deriving Show

-- | Gap is that gap between previous lost packet and latest one in Ack
-- Frame.
type Gap = Int

-- | AckTimeStamp is TimeStamp represent in Ack Frame.
data AckTimeStamp = AckTimeStamp [(Gap, QUICTime)]
                  deriving Show

type PacketNumber = Integer

type ConnectionId = Integer

type QUICVersion = Int32

-- TODO: on LongPacket, Payload filed is not suitable. it will be removed.
-- LongHeaderPacket should include the payload and LongHeaderPacket is renamed to good one.
data Packet = LongPacket  Header LongPacketPayload
            | ShortPacket Header ShortPacketPayload
            deriving Show


-- | ErrorCode is exported error code api.
-- TODO: check it is good?
data ErrorCode = ApplicationErrorCode Int
               | HostLocalErrorCode Int
               | QUICErrorCode QUICError
               | CrypotograhicError Int
               deriving Show

-- | QUIC Error Code
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




-- | Mode, the context is client or server.
-- | internal use.
data Mode = Client | Server
          deriving Show

