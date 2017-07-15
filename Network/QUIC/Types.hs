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
data PacketContext = PacketContext { contextPacketNumberSize :: Int }
             deriving (Show, Eq)

data Header = LongHeader ConnectionId PacketNumber QUICVersion
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

data StreamSize = Stream1Byte | Stream2Byte | Stream3Byte
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

data AckTimeStamp = AckTimeStamp [QUICTime]
                  deriving Show

data Frame = Stream !StreamId !Offset !(Maybe Int)
           | Ack !(Maybe Int) !Int !PacketNumber !QUICTime !QUICTime !AckBlock !AckTimeStamp
           | MaxData !Int64
           | MaxStreamData !StreamId !Int
           | MaxStreamId !StreamId
           | Blocked
           | StreamBlocked !StreamId
           | StreamIdNeeded
           | RstStream !StreamId !QUICErrorCode !Offset
           | Padding
           | Ping
           | NewConnectionId !Int !ConnectionId
           | ConnectionClose !QUICErrorCode !ByteString
           | Goaway !StreamId !StreamId
           deriving Show


type PacketNumber = Integer

type ConnectionId = Integer

type QUICVersion = Int32
data Packet = Packet Header Payload
            deriving Show
type Payload = [Frame]
data QUICErrorCode = ADMITTED
    deriving Show

data QUICError = QUICErrorCode QUICErrorCode
               | Otherwise Int
               deriving Show

type QUICResult a = Either QUICError a

