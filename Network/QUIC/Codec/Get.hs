module Network.QUIC.Codec.Get
  (
  runGetOrFailWithError
  , getLongHeader
  , getShortHeader

  , getQUICTime
  , getQUICVersion
  , getQUICVersions
  , getPacketNumber
  , getConnectionId
  , getOffset
  , getErrorCode

  , getNewConnectionId
  , getVersionNegotiation

  , getStreamFrame
  , getAckFrame
  , getGoawayFrame
  , getMaxStreamDataFrame
  , getMaxStreamIdFrame
  , getMaxDataFrame
  , getRstStreamFrame
  , getStreamBlockedFrame
  , getConnectionCloseFrame

  )
  where
import           Data.Binary.Get

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Data.Bits
import           Data.Int
import           Data.Word

import           Network.QUIC.Codec.Internal
import           Network.QUIC.Types
import qualified Network.QUIC.UFloat16       as UF

-- | runGetOrFailWithError
runGetOrFailWithError :: ByteString -> Get a -> QUICError ->  QUICResult (a, ByteString)
runGetOrFailWithError bs f e = case (runGetOrFail f $ LBS.fromStrict bs) of
    (Right (bs', _, p)) -> Right (p, LBS.toStrict bs')
    _                   -> Left e

-- | getQUICVersion
getQUICVersion :: Get QUICVersion
getQUICVersion = fromIntegral <$> getInt32be

-- | getQUICVersions
getQUICVersions :: Get [QUICVersion]
getQUICVersions =  isEmpty >>= \ b -> if b then return [] else get
  where
     get = do
        v <- getQUICVersion
        vs <- getQUICVersions
        return (v:vs)

-- | getQUICTime
getQUICTime :: Get QUICTime
getQUICTime = fromIntegral <$> getInt32be

-- | getOffset
getOffset :: OffsetSize -> Get Offset
getOffset NoExistOffset = return 0
getOffset Offset2Byte   = fromIntegral <$> getInt16be
getOffset Offset4Byte   = fromIntegral <$> getInt32be
getOffset Offset8Byte   = fromIntegral <$> getInt64be

-- | getStreamId
getStreamId :: StreamSize -> Get StreamId
getStreamId ss = case ss of
  Stream1Byte -> fromIntegral <$> getInt8
  Stream2Byte -> fromIntegral <$> getInt16be
  Stream3Byte -> fromIntegral <$> getInt24be
  Stream4Byte -> fromIntegral <$> getInt32be

-- | getConnectionId
getConnectionId :: Get ConnectionId
getConnectionId = fromIntegral <$> getInt64be

-- | getPacketNumber
getPacketNumber :: PacketNumberSize ->  Get PacketNumber
getPacketNumber p =  case p of
  PacketNumber1Byte -> fromIntegral <$> getInt8
  PacketNumber2Byte -> fromIntegral <$> getInt16be
  PacketNumber4Byte -> fromIntegral <$> getInt32be

-- | getErrorCode
getErrorCode :: Get ErrorCode
getErrorCode = intToErrorCode . fromIntegral <$> getInt32be


--
--
-- Header
--
--

-- | getLongHeader
getLongHeader :: LongHeaderType -> Get Header
getLongHeader h =  LongHeader h
        <$> getConnectionId
        <*> getPacketNumber PacketNumber4Byte
        <*> getQUICVersion

-- | getShortHeader
getShortHeader :: Word8 -> Get Header
getShortHeader w = ShortHeader <$> getConnIdMaybe w <*> getPacketNumber (toPacketNumberSize w)
  where
    getConnIdMaybe w = if hasConn w then Just <$> getConnectionId else return Nothing
    hasConn w = w .|. 0x40 == 0x40
    keyphase w = w .|. 0x20 == 0x20

-- Long Packet Payload

getVersionNegotiation :: Get LongPacketPayload
getVersionNegotiation = VersionNegotiation <$> getQUICVersion <*> getQUICVersions

--
--
--  Frame
--
--
-- | getStreamFrame

getMaxDataFrame :: Get Frame
getMaxDataFrame = MaxData <$> getInt64be

getMaxStreamDataFrame :: StreamSize -> Get Frame
getMaxStreamDataFrame ss = MaxStreamData <$> getStreamId ss <*> getMaxStreamData
  where
    getMaxStreamData = fromIntegral <$> getInt64be

getMaxStreamIdFrame :: StreamSize ->  Get Frame
getMaxStreamIdFrame ss = MaxStreamId <$> getStreamId ss

getStreamBlockedFrame :: StreamSize -> Get Frame
getStreamBlockedFrame ss = StreamBlocked <$> getStreamId ss

getRstStreamFrame :: StreamSize -> OffsetSize-> Get Frame
getRstStreamFrame ss oo = RstStream <$> getStreamId ss <*> getErrorCode <*> getOffset oo

getNewConnectionId :: Get Frame
getNewConnectionId = NewConnectionId <$> (fromIntegral <$> getInt16be) <*> getConnectionId

getConnectionCloseFrame :: Get Frame
getConnectionCloseFrame = ConnectionClose <$> getErrorCode  <*> getMsg
getMsg = getInt32be >>= getmsg
  where
    getmsg 0 = LBS.toStrict <$> getRemainingLazyByteString
    getmsg n = getByteString $ fromIntegral n

getStreamFrame ctx f ss oo d = Stream  <$> getStreamId ss <*> getOffset oo <*> getStreamData d
  where
    getStreamData :: Bool -> Get ByteString
    getStreamData d = do
     n <- if d then fromIntegral <$> getInt16be else return 0
     if n == 0 then return BS.empty else getByteString $ fromIntegral n

getGoawayFrame :: StreamSize ->  Get Frame
getGoawayFrame ss = Goaway <$> getStreamId ss <*> getStreamId ss

-- | getAckFrame
getAckFrame  :: Bool -> LAckSize -> AckBlockLengthSize -> Get Frame
getAckFrame n lack abl = getNumBlock n >>=
                        \ nblock ->  getNTS >>=
                        \ nstamps -> getLAck lack >>=
                        \ l ->
  Ack l <$> getAckDelay <*> getAckBlocksMaybe nblock abl <*> getAckTimeStamps l nstamps
  where
    getAckBlocksMaybe :: Maybe Int -> AckBlockLengthSize -> Get AckBlock
    getAckBlocksMaybe Nothing abl        =  getAckBlocks 0 abl
    getAckBlocksMaybe (Just nblocks) abl =  getAckBlocks nblocks abl

    getNumBlock :: Bool -> Get (Maybe Int)
    getNumBlock n = if n then Just <$> fromIntegral <$> getInt8 else return Nothing
    getNTS :: Get Integer
    getNTS = fromIntegral <$> getWord8
    getLAck :: LAckSize -> Get PacketNumber
    getLAck lack =  case lack of
                      LAck1Byte -> fromIntegral <$> getInt8
                      LAck2Byte -> fromIntegral <$> getInt16be
                      LAck4Byte -> fromIntegral <$> getInt32be
                      LAck8Byte -> fromIntegral <$> getInt64be

-- | getAckDelay
-- TODO:  Time Format
-- ref :https://github.com/quicwg/base-drafts/issues/109
-- reference implementation :
-- https://gist.github.com/mikkelfj/64deb01b86d68f3d7aacff4b113c22d8
getAckDelay :: Get  AckTimeDelta
getAckDelay =  fromIntegral <$> getInt16be

-- | getAckBlockLength
getAckBlockLength :: AckBlockLengthSize -> Get PacketNumber
getAckBlockLength abl = case abl of
    AckBlock1Byte -> fromIntegral <$> getInt8
    AckBlock2Byte -> fromIntegral <$> getInt16be
    AckBlock4Byte -> fromIntegral <$> getInt32be
    AckBlock6Byte -> fromIntegral <$> getInt48be

-- | getAckBlocks
getAckBlocks :: Int -> AckBlockLengthSize -> Get AckBlock
getAckBlocks nblock abl = getAckBlockLength abl >>= (\ first -> getAckBlock abl first)
  where
    getAckBlock abl first = do
      pn0   <-  getAckBlockLength abl
      rest  <- getRestBlock abl (pn0 - first)
      return $ AckBlock ( [pn0, pn0-1.. pn0 - first ] ++  rest )
      where
        getGap = fromIntegral <$> getInt8
        getRestBlock :: AckBlockLengthSize -> PacketNumber ->  Get [PacketNumber]
        getRestBlock abl pn
          | (pn <= 0) = return []
          | otherwise = do
          gap <- getGap
          len <- getAckBlockLength abl
          let pn' = (fromIntegral pn) - gap
              pn''  = pn' - len
              cont :: [PacketNumber]
              cont =  [pn', pn'-1..pn'']
          rest <- getRestBlock abl pn''
          return (cont ++ rest)

-- | getAckTimeStamps
getAckTimeStamps :: PacketNumber  -- Largest Acked
                -> Integer            -- Delta Largest Acked
                ->  Get AckTimeStamp
getAckTimeStamps lack n = do
    delta0  <- getDelta
    stamp0  <- getQUICTime
    rest    <- getAckTimeStamp lack stamp0 (n - 1)
    let first = (lack - delta0, stamp0)
    return $ AckTimeStamp (first : rest)
  where
    getDelta = fromIntegral <$> getInt8
    getQUICTimeDelta = getAckDelay
    addQUICTimeDelta :: QUICTime -> AckTimeDelta -> QUICTime
    addQUICTimeDelta time delta = undefined

    getAckTimeStamp :: PacketNumber -> QUICTime -> Integer -> Get [(PacketNumber, QUICTime)]
    getAckTimeStamp _     time 0 = return []
    getAckTimeStamp lack  time n = do
      gap <- getDelta
      diff <- getQUICTimeDelta
      let time' = addQUICTimeDelta time diff
      rest <- getAckTimeStamp lack time' (n - 1)
      return ((lack - gap, time') : rest)



--- Internal

getInt24be :: Get Int32
getInt24be = do
    mb <- fromIntegral <$> getInt16be
    ml <- fromIntegral <$> getInt8
    return $ shiftL mb 8 + ml

getInt48be :: Get Int64
getInt48be = do
    mb <- fromIntegral <$> getInt32be
    ml <- fromIntegral <$> getInt16be
    return $ shiftL mb 16 + ml



getIntNbyte :: Int -> Get Int
getIntNbyte 0 = return 0
getIntNbyte n = foldl f 0 <$> list
  where
    f :: Int -> (Int8, Int) -> Int
    f n (x,i) = n + (shiftL (i * 8) $ toInt x)

    list :: Get [(Int8, Int)]
    list = (\xs -> zip xs [0..])  <$> (sequence $ replicate n getInt8)

    toInt = fromIntegral . toInteger

