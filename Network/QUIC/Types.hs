{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.QUIC.Types
  (
    QUICResult
  , ErrorCode(..)
  , QUICError(..)
  , Context(..)

  , Packet(..)
  , LongHeaderType(..)
  , HeaderType(..)
  , Header(..)
  , LongPacketPayload(..)
  , FrameType(..)
  , Frame(..)

  , PacketNumber
  , StreamId
  , ConnectionId
  , Offset
  , QUICVersion
  , QUICTime(..)
  , encodeQUICTime
  , decodeQUICTime
  , diffQUICTime
  , addQUICTime

  -- internal
  , AckBlock(..)
  , AckTimeStamp(..)
  , AckTimeDelta
  , Gap
  , DecodeContext(..)
  , EncodeContext(..)
  , LongPacketContext(..)
  , PacketNumberSize(..)
  , StreamSize(..)
  , OffsetSize(..)
  , AckBlockLengthSize(..)
  , LAckSize(..)
  , Mode(..)
  )
  where

import qualified Data.ByteString       as BS
import           Data.Default.Class
import           Data.Int
import           Data.Time.Clock

import           Network.QUIC.UFloat16

-- | QUICResult is result type in the QUIC protocol context.
type QUICResult a = Either QUICError a


-- | Context is type that indicates connection Context
data Context = Context { contextMode              :: Mode
                       , contextNegotiatedVersion :: Maybe QUICVersion
                       , contextAcceptedData      :: BS.ByteString}
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
                | ClientInitial ![Frame]
                | ServerStatelessRetry ![Frame]
                | ServerCleartext ![Frame]
                | ClientCleartext ![Frame]
                | ZeroRTTProtected ![Frame]
                | OneRTTProtectedKeyPhaseZero ![Frame]
                | OneRTTProtectedKeyPhaseOne ![Frame]
                | PublicReset ![Frame]
                deriving (Show)

-- | ShortPacketPayload
type ShortPacketPayload = [Frame]

-- | Frame
-- TODO: note commnets.
data Frame = Stream !StreamId !Offset !BS.ByteString
           | Ack !PacketNumber !AckTimeDelta !AckBlock !AckTimeStamp
           | MaxData !Int64
           | MaxStreamData !StreamId !Int64
           | MaxStreamId !StreamId
           | Blocked
           | StreamBlocked !StreamId
           | StreamIdNeeded
           | RstStream !StreamId !ErrorCode !Offset
           | Padding
           | Ping
           | NewConnectionId !Int16 !ConnectionId -- Sequence ConnectionId
           | ConnectionClose !ErrorCode !BS.ByteString -- ErrorCode ErrorMessage
           | Goaway !StreamId !StreamId
           deriving Show

-- | AckBlock is Blocks that is recived  in Ack Frame.
data AckBlock = AckBlock [PacketNumber]
              deriving Show

-- | Gap is that gap between previous lost packet and latest one in Ack
-- Frame.
type Gap = Int

-- | AckTimeStamp is TimeStamp represent in Ack Frame.
data AckTimeStamp = AckTimeStamp [(PacketNumber, QUICTime)]
                  deriving Show

type AckTimeDelta = UFloat16

data PacketNumberSize = PacketNumber1Byte | PacketNumber2Byte | PacketNumber4Byte
                      deriving Show

data DecodeContext = DecodeContext { decodeContextStreamSize :: StreamSize
                                   , decodeContextOffsetSize :: OffsetSize
                                   , decodeContextPacketNumberSize :: PacketNumberSize
                                   , decodeContextLongPacketContext :: Maybe LongPacketContext}
                                   deriving Show

data EncodeContext = EncodeContext { encodeContextPacketNumberSize  :: PacketNumberSize
                                   , encodeContextStreamSize        :: StreamSize
                                   , encodeContextOffsetSize        :: OffsetSize
                                   , encodeContextStreamFin         :: Bool
                                   , encodeContextStreamHasData     :: Bool }
                                   deriving Show

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
data AckBlockLengthSize = AckBlock1Byte | AckBlock2Byte | AckBlock4Byte | AckBlock6Byte
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
type StreamId = Int32

-- | Offset is type that indicate offset from Head of Data in Stream Frame
type Offset = Integer

-- QUICTime is moved to Network.QUIC.Time module


type PacketNumber = Int64

type ConnectionId = Int64

type QUICVersion = Int32

-- TODO: on LongPacket, Payload filed is not suitable. it will be removed.
-- LongHeaderPacket should include the payload and LongHeaderPacket is renamed to good one.
data Packet = LongPacket  Header LongPacketPayload
            | ShortPacket Header ShortPacketPayload
            deriving Show

-- | QUICTime
-- TODO: See [draft-ietf-quic-transport-04] Section 8.2.2.1 "Time Format"
-- We Must implement IEEE74 like time format and the utility.
-- 16bit unsinged float.
-- with
-- mantissa 11 bit
-- exponent 5 bit
-- time in microseconds
newtype QUICTime =  QUICTime Int32
                   deriving (Show, Eq, Ord, Enum)

instance Real QUICTime where
    toRational = undefined


instance Integral QUICTime where
    quotRem = undefined
    toInteger = undefined

instance Num QUICTime where
    (+) = addQUICTime
    (-) = subQUICTime
    (*) = multiQUICTime
    abs = absQUICTime
    signum = signumQUICTime
    fromInteger = undefined


encodeQUICTime :: Int32 -> QUICTime
encodeQUICTime i = undefined

decodeQUICTime :: QUICTime -> Int
decodeQUICTime t = undefined

addQUICTime :: QUICTime -> QUICTime -> QUICTime
addQUICTime l r = encodeQUICTime $ l' + r'
  where
    l' = fromIntegral $ decodeQUICTime l
    r' = fromIntegral $ decodeQUICTime r

subQUICTime :: QUICTime -> QUICTime -> QUICTime
subQUICTime l r = encodeQUICTime $ l' - r'
  where
    l' = fromIntegral $ decodeQUICTime l
    r' = fromIntegral $ decodeQUICTime r

diffQUICTime :: QUICTime -> QUICTime -> AckTimeDelta
diffQUICTime l r  = undefined

multiQUICTime :: QUICTime -> QUICTime -> QUICTime
multiQUICTime l r = encodeQUICTime $ l' * r'
  where
    l' = fromIntegral $ decodeQUICTime l
    r' = fromIntegral $ decodeQUICTime r

absQUICTime :: QUICTime -> QUICTime
absQUICTime t = encodeQUICTime $ abs $ fromIntegral $ decodeQUICTime t

signumQUICTime = undefined

quicTimeToInteger :: QUICTime -> Int
quicTimeToInteger = undefined

addQUICTimeAckDelta :: QUICTime -> AckTimeDelta -> QUICTime
addQUICTimeAckDelta time delta = undefined

toUTCTime :: QUICTime -> UTCTime
toUTCTime = undefined

fromUTCTime :: UTCTime -> QUICTime
fromUTCTime = undefined



--
--
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

