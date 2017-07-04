module Network.QUIC
  (
    ManagerSetting(..),
    Manager(..),
    newManager,
    closeManager,
    withManager,

    Packet(..),
    Header(..),
    Frame(..),

    encode,
    decode,

    TransportParameters(..),
    TransportParameter(..),
  )
  where
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.HashTable.IO     as HashTbl
import           Data.Int
import           Data.Maybe
import           Data.Time.Clock
import           Data.Word
import qualified Network.QUIC.Internal as I
import           Network.Socket

data ManagerSetting = ManagerSetting
                    deriving Show


-- Manager API
-- default ManageSetting configuration paramater
defaultManagerSetting = ManagerSetting

-- socket, connection stream, key abstraction.
data Manager = Manager { mgrConns :: [Int]
                        ,mgrPeer  :: Int
                        ,mgrSock  :: Socket
                        -- hash from 4 tuple to socket
                       }
             deriving Show

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager = return undefined

-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager mgr = undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager = undefined

-- Header


-- Header is header format.
-- while reconnection or handshake, we should  use long Header format.
-- otherwise, we must use short Header format.
data Header = LongHeader ConnectionId PacketNumber QUICVersion
            | ShortHeader (Maybe ConnectionId) PacketNumber
            deriving Show

data HeaderType = LongHeaderType | ShortHeaderType
                deriving Show

isHeaderType :: Word8 -> HeaderType
isHeaderType b
  | (b == 0x80) = LongHeaderType
  | otherwise =  ShortHeaderType


data LongHeaderPacket = VersionNegotiation  QUICVersion [QUICVersion]
                | ClientInitial
                | ServerStatelessRetry
                | ServerCleartext
                | ClientCleartext
                | ZeroRTTProtected
                | OneRTTProtected
                | OneRTTProtectedKeyPhaseZero
                | OneRTTProtectedKeyPhaseOne
                | PublicReset

data FrameType = StreamType Word8
               | AckType Word8
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

hasConnectionId :: Word8 -> Bool
hasConnectionId = undefined

hasKeyPhase :: Word8 -> Bool
hasKeyPhase = undefined
bitToFrameType :: Word8 -> Maybe FrameType
bitToFrameType 0x00 = Just PaddingType
bitToFrameType 0x01 = Just RstStreamType
bitToFrameType 0x02 = Just ConnectionCloseType
bitToFrameType 0x03 = Just GoawayType
bitToFrameType 0x04 = Just MaxDataType
bitToFrameType 0x05 = Just MaxStreamDataType
bitToFrameType 0x06 = Just MaxStreamIdType
bitToFrameType 0x07 = Just PingType
bitToFrameType 0x08 = Just BlockedType
bitToFrameType 0x09 = Just StreamBlockedType
bitToFrameType 0x0a = Just StreamIdNeededType
bitToFrameType 0x0b = Just NewConnectionType
bitToFrameType -31 = Just StreamType -- 0xa0 - 0xbf
bitToFrameType -63  = Just AckType -- 0xc0 - 0xff

type StreamId = Int32
type Offset = Int64

data QUICTime = QUICTime
             deriving Show

data AckBlock = AckBlock [PacketNumber]
              deriving Show

data AckTimeStamp = AckTimeStamp [QUICTime]
                  deriving Show

data Frame = Stream !StreamId !Offset !Int !ByteString
           | Ack (Maybe Int) Int PacketNumber QUICTime QUICTime AckBlock AckTimeStamp
           | MaxData Int64
           | MaxStreamData StreamId Int
           | MaxStreamId StreamId
           | Blocked
           | StreamBlocked StreamId
           | StreamIdNeeded
           | RstStream StreamId QUICErrorCode Offset
           | Padding
           | Ping
           | NewConnectionId Int ConnectionId
           | ConnectionClose QUICErrorCode !ByteString
           | Goaway StreamId StreamId
           deriving Show

decodeHeader :: ByteString -> QUICResult (Header, ByteString)
decodeHeader bs = undefined

encodeHeader :: Header -> ByteString
encodeHeader hdr = undefined

decodeFrames :: Header -> ByteString -> QUICResult [Frame]
decodeFrames hdr bs = undefined
  where
    decodeFrames' = undefined

encodeFrames :: [Frame] -> ByteString
encodeFrames fs = undefined

decode :: ByteString -> QUICResult Packet
decode bs = decode' $ decodeHeader bs
    where
    decode' (Left e) = Left e
    decode' (Right (hdr,bs')) = case (decodeFrames hdr bs') of
              (Right fs) -> Right (Packet hdr fs)
              (Left e)   -> Left e


encode :: Packet -> ByteString
encode (Packet hdr fs) = encodeHeader hdr `BS.append` encodeFrames fs


type PacketNumber = Integer
type ConnectionId = Int64
type QUICVersion = Int32
data Packet = Packet Header Payload
            deriving Show
type Payload = [Frame]
data QUICErrorCode = ADMITTED
    deriving Show

quicErrorCodeToInt :: QUICErrorCode -> Int
quicErrorCodeToInt _ = undefined

quicErrorToInt :: QUICError -> Int
quicErrorToInt (QUICErrorCode code) = quicErrorCodeToInt code
quicErrorToInt (Otherwise i)        = i

data QUICError = QUICErrorCode QUICErrorCode
               | Otherwise Int
               deriving Show

type QUICResult a = Either QUICError a


-- TODO: porting from draft code like c++.  It should be replaced more suitable data format.
-- TODO: it demands to adapt TLS. they are requred TLS extension "quic_transport_paramters".


data TransportParameterId = TransParamInitialMaxStreamData
                          | TransParamInitialMaxData
                          | TransParamInitialMaxStreamId
                          | TransParamIdleTimeout
                          | TransParamTruncateConnectionId
                          | TransParamMaxPacketSize
                          | TransParamUnknown Int
                          deriving Show

data TransportParameter = TransportParameter {
                          transParamId    :: TransportParameterId
                        , transParamValue :: ByteString

                        }
                        deriving Show

data TransportParameters = TransportParameterss {
                        -- client hello
                          transParamNegotiatedVersion  :: Maybe QUICVersion
                        , transParamsInitialVersion    :: Maybe QUICVersion
                        -- encrypted extensions
                        , transParamsSupportedVerisons :: [QUICVersion]
                        -- default
                        , transParamsParams            :: [TransportParameter]
                        }
                        deriving Show

-- Should we  replace TransportParameters?
data Config = Config { configPMTU :: Int }
            deriving Show

-- the number that library support protocol version.
supportedVersion :: [Int32]
supportedVersion = [0x0d,
                    0x0e
                   ]

-- latest number that the libary support
defaultVersion :: Int32
defaultVersion = 0x0e

data Mode = Client | Server
          deriving Show

-- Internaly Manager
data EndPoint = EndPoint { endPointMode :: Mode
              }
              deriving Show


-- TODO: QUIC WG discuss to simplify Stream State.
-- it should obey the result of discussion.
data Handle = Idle | Open | RemoteHalfClose | LocalHalfClose | Closed
            deriving Show

-- Prioritazation is stream prioritazation abstraction
data Priority = Priority {}
                    deriving Show

defaultPrioritazation :: Priority
defaultPrioritazation = undefined

incrPriority :: Priority
incrPriority = undefined

decrPriority :: Priority
decrPriority = undefined

-- QUIC time format

parseTime :: ByteString -> QUICTime
parseTime bs = undefined
  where
    mantissa = undefined
    exponent = undefined

toUTCTime :: QUICTime -> UTCTime
toUTCTime = undefined

fromUTCTime :: UTCTime -> QUICTime
fromUTCTime = undefined

-- QUIC Version  Negtiaton

isVaildVersion :: QUICVersion -> Bool
isVaildVersion = undefined

isSupportedversion :: QUICVersion -> Bool
isSupportedversion = undefined

supportedVersions :: [QUICVersion] -> [QUICVersion]
supportedVersions = undefined

-- Init STREAM

newStreamFrame :: Frame
newStreamFrame = undefined
