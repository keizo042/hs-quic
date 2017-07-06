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

-- Tesing
    testDataMap,
    testStreamData
  )
  where
import           Control.Monad
import qualified Data.Binary           as B
import qualified Data.Binary.Get       as Get
import qualified Data.Binary.Put       as Put
import           Data.Bits
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
import qualified Network.Socket        as S
import qualified Network.TLS           as TLS

data ManagerSetting = ManagerSetting { managerSettingPort :: Int
                    , managerSettingHost                  :: !String}
                    deriving Show


-- Manager API
-- default ManageSetting configuration paramater
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- socket, connection stream, key abstraction.
data Manager = Manager { mgrConns :: [Int]
                        ,mgrPeer  :: Int
                        ,mgrSock  :: S.Socket
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
                deriving (Show, Eq)

isHeaderType :: Word8 -> HeaderType
isHeaderType b
  | (b == 0x80) = LongHeaderType
  | otherwise =  ShortHeaderType

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

bitToLongHeaderType :: Word8 -> Maybe LongHeaderType
bitToLongHeaderType w
  | w == 1 = Just VersionNegotiationType
  | w == 2 = Just ClientInitialType
  | w == 3 = Just ServerStatelessRetryType
  | w == 4 = Just ServerCleartextType
  | w == 5 = Just ClientCleartextType
  | w == 6 = Just ZeroRTTProtectedType
  | w == 7 = Just OneRTTProtectedKeyPhaseZeroType
  | w == 8 = Just OneRTTProctectedKeyPhaseOneType
  | w == 9 = Just PublicResetType
  | otherwise = Nothing

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
hasConnectionId w = w .&. 0x40 ==  0x40

hasKeyPhase :: Word8 -> Bool
hasKeyPhase w = w .&. 0x20 == 0x20

bitToFrameType :: Word8 -> Maybe FrameType
bitToFrameType w = case (w .&. 0x1f) of
      0x00 -> Just PaddingType
      0x01 -> Just RstStreamType
      0x02 -> Just ConnectionCloseType
      0x03 -> Just GoawayType
      0x04 -> Just MaxDataType
      0x05 -> Just MaxStreamDataType
      0x06 -> Just MaxStreamIdType
      0x07 -> Just PingType
      0x08 -> Just BlockedType
      0x09 -> Just StreamBlockedType
      0x0a -> Just StreamIdNeededType
      0x0b -> Just NewConnectionType
      -31  -> Just StreamType -- 0xa0 - 0xbf
      -63  -> Just AckType -- 0xc0 - 0xff

type StreamId = Int

putStreamId :: StreamId -> Put.Put
putStreamId = undefined

getStreamId :: Int -> Get.Get StreamId
getStreamId 1 = I.getInt8
getStreamId 2 = I.getInt16
getStreamId 3 = I.getInt24
getStreamId 4 = I.getInt32

type Offset = Integer

getOffset :: Int -> Get.Get Offset
getOffset 0 = return 0
getOffset 2 = fromIntegral <$> I.getInt16
getOffset 4 = fromIntegral <$> I.getInt32
getOffset 8 = fromIntegral <$> I.getInt64

putOffset :: Offset -> Put.Put
putOffset offset = undefined

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

decodeHeader :: ByteString -> QUICResult (Header, ByteString)
decodeHeader bs = undefined
  where
    decodeLongHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeLongHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                         Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                         Left _               -> undefined
      where
        decode' :: Get.Get Header
        decode' = undefined
    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                          Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                          Left _               -> undefined
      where
        decode' :: Get.Get Header
        decode' = undefined

encodeHeader :: Header -> ByteString
encodeHeader hdr = undefined

decodeFrames :: Header -> ByteString -> QUICResult [Frame]
decodeFrames hdr bs = case (decodeFrame hdr bs) of
                        Right (f,bs') -> case (decodeFrames hdr bs') of
                                     Right fs -> Right (f : fs)
                                     Left e   -> Left e
                        Left e  -> Left e


decodeFrame :: Header -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame hdr bs = case (bitToFrameType $ BS.head bs) of
                       Nothing    -> undefined
                       Just ft  -> case ft of
                         (StreamType bit)     -> decodeStreamFrame hdr bit bs
                         (AckType bit)        -> decodeAckFrame hdr bit bs
                         MaxDataType          -> decodeMaxDataFrame hdr bs
                         MaxStreamDataType    -> decodeMaxStreamDataFrame hdr bs
                         MaxStreamIdType      -> decodeMaxStreamIdFrame hdr bs
                         StreamBlockedType    -> decodeStreamBlockedFrame hdr bs
                         StreamIdNeededType   -> decodeStreamIdNeededFrame hdr bs
                         RstStreamType        -> decodeRstStreamFrame hdr bs
                         PaddingType          -> decodePaddingFrame hdr bs
                         PingType             -> decodePingFrame hdr bs
                         NewConnectionType    -> decodeNewConnectionIdFrame hdr bs
                         ConnectionCloseType  -> decodeConnectionCloseFrame hdr bs
   where
     decodeStreamFrame :: Header -> Word8 -> ByteString -> QUICResult (Frame, ByteString)
     decodeStreamFrame  hdr bit bs = case (Get.runGetOrFail decode'  $ LBS.fromStrict bs) of
                                       (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                       _ -> undefined

        where
           chkStreamId :: Word8 -> Int
           chkStreamId = undefined

           chkOffset :: Word8 -> Int
           chkOffset = undefined

           sn :: Int
           sn = undefined

           ofn :: Int
           ofn = undefined
           decode' :: Get.Get Frame
           decode' = undefined -- Just <$> Stream <*> (getStreamId sn) <*> (getOffset ofn) <*> I.getInt16
     decodeAckFrame hdr bit bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeMaxDataFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeMaxStreamDataFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeMaxStreamIdFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeBlockedFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeStreamBlockedFrame  hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeStreamIdNeededFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeRstStreamFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodePaddingFrame _ bs = Right (Padding, BS.tail bs)
     decodePingFrame _ bs = Right (Ping, BS.tail bs)
     decodeNewConnectionIdFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeConnectionCloseFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeGoawayFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined



encodeFrames :: [Frame] -> ByteString
encodeFrames []     = BS.empty
encodeFrames (f:fs) = encodeFrame f `BS.append` encodeFrames fs

encodeFrame :: Frame -> ByteString
encodeFrame f = LBS.toStrict  undefined
  where
    encodeStramFrame = undefined
    encodeAckFrame = undefined
    encodeAck = undefined

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

getPacketNumber = undefined

type ConnectionId = Integer

putConnectionId :: ConnectionId -> Put.Put
putConnectionId = Put.putWord64be . fromIntegral

getConnectionId :: Get.Get ConnectionId
getConnectionId = fromIntegral <$> I.getInt64

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

intToQUICError :: Int -> QUICError
intToQUICError = undefined

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

data TransportParameters = TransportParametersClientHello QUICVersion QUICVersion [TransportParameter]
                         | TransportPArametersEncryptedExtensions [QUICVersion] [TransportParameter]
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

toUTCTime :: QUICTime -> Clock.UTCTime
toUTCTime = undefined

fromUTCTime :: Clock.UTCTime -> QUICTime
fromUTCTime = undefined

-- QUIC Version  Negtiaton

isVaildVersion :: QUICVersion -> Bool
isVaildVersion = undefined

isSupportedversion :: QUICVersion -> Bool
isSupportedversion = undefined

supportedVersions :: [QUICVersion] -> [QUICVersion]
supportedVersions = undefined

-- Init STREAM
-- STREAM ID 0
-- padded 1280 frame.

initStream = undefined

newStreamFrame :: Frame
newStreamFrame = undefined


-- Obtain Stream Data

waitData :: StreamId -> ByteString
waitData = undefined

-- Stream Data Internaly

insertStreamData = undefined

-- Stream Data Map , Key: Stream Id / Value: (Packet Number, Data)
-- the data is specific Stream Id Data

type StreamData = M.Map Offset ByteString

type DataMap = M.Map StreamId StreamData

initDataMap :: DataMap
initDataMap = M.empty

insertDataMap :: DataMap
              -> StreamId
              -> Offset
              -> ByteString
              -> DataMap
insertDataMap m sid ofs bs = modify' sid ofs bs sd m
    where
      sd = lookup' sid m
      lookup' ::  StreamId -> DataMap -> (Maybe StreamData)
      lookup' sid' m' = M.lookup sid' m'

      modify' :: StreamId -> Offset -> ByteString -> (Maybe StreamData) -> DataMap -> DataMap
      modify' sid' ofs' bs' sd0 m' = M.insert sid' sd1 m'
        where
          sd1 = case sd0 of
                    (Just sd) ->  M.insert ofs' bs' sd
                    Nothing   ->  M.insert ofs' bs' M.empty

dropDataMap :: StreamId -> DataMap -> DataMap
dropDataMap sid m = M.delete sid m

takeDataMap :: StreamId
            -> DataMap
            -> (DataMap, Maybe ByteString)
takeDataMap sid m = case (M.lookup sid m) of
                      (Just sd) -> (M.delete sid m, Just $ toBS sd)
                      (Nothing) -> (m, Nothing)
                where
                  toBS :: StreamData -> ByteString
                  toBS sd = foldl BS.append BS.empty $ map snd $ M.toList sd
                  f :: ByteString -> (PacketNumber, ByteString) -> ByteString
                  f b (k,v) = b `BS.append` v


-- send Client Hello with 1RTT
sendClientHello = undefined

-- recv Client Hello with 1RTT
recvClinetHello = undefined

sendServerHello = undefined

-- recv Server Hello with 1RTT
recvServerHello = undefined

-- send 1 RTT Key exchange
exchange1RTTKey = undefined

--
-- testing
--
testDataMap = undefined
testStreamData = undefined
