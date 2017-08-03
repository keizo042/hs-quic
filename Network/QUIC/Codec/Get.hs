module Network.QUIC.Codec.Get
  where
import           Data.Binary.Get

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Data.Bits
import           Data.Int
import           Data.Word

import           Network.QUIC.Codec.Internal
import qualified Network.QUIC.Internal       as I
import           Network.QUIC.Time
import           Network.QUIC.Types
import qualified Network.QUIC.UFloat16       as UF

runGetOrFailWithError :: ByteString -> Get a -> QUICError ->  QUICResult (a, ByteString)
runGetOrFailWithError bs f e = case (runGetOrFail f $ LBS.fromStrict bs) of
    (Right (bs', _, p)) -> Right (p, LBS.toStrict bs')
    _                   -> Left e



getQUICVersion :: Get QUICVersion
getQUICVersion = fromIntegral <$> I.getInt32

getQUICVersions :: Get [QUICVersion]
getQUICVersions =  isEmpty >>= \ b -> if b then return [] else get
  where
     get = do
        v <- getQUICVersion
        vs <- getQUICVersions
        return (v:vs)

getQUICTime :: Get QUICTime
getQUICTime = fromIntegral <$> getInt32be

getOffset :: OffsetSize -> Get Offset
getOffset NoExistOffset = return 0
getOffset Offset2Byte   = fromIntegral <$> I.getInt16
getOffset Offset4Byte   = fromIntegral <$> I.getInt32
getOffset Offset8Byte   = fromIntegral <$> I.getInt64

getStreamId :: StreamSize -> Get StreamId
getStreamId ss = fromIntegral <$> case ss of
                                            Stream1Byte -> I.getInt8
                                            Stream2Byte -> I.getInt16
                                            Stream3Byte -> I.getInt24
                                            Stream4Byte -> I.getInt32

getConnectionId :: Get ConnectionId
getConnectionId = fromIntegral <$> I.getInt64

getPacketNumber :: PacketNumberSize ->  Get PacketNumber
getPacketNumber p = fromIntegral <$> case p of
  PacketNumber1Byte -> I.getInt8
  PacketNumber2Byte -> I.getInt16
  PacketNumber4Byte -> I.getInt32

-- | getErrorCode
getErrorCode :: Get ErrorCode
getErrorCode = intToErrorCode . fromIntegral <$> I.getInt32


--
--
-- Header
--
--

getLongHeader :: LongHeaderType -> Get Header
getLongHeader h =  LongHeader h
        <$> getConnectionId
        <*> getPacketNumber PacketNumber4Byte
        <*> getQUICVersion

getShortHeader :: Word8 -> Get Header
getShortHeader w = ShortHeader <$> getConnIdMaybe w <*> getPacketNumber (toPacketNumberSize w)
  where
    getConnIdMaybe w = if hasConn w then Just <$> getConnectionId else return Nothing
    hasConn w = w .|. 0x40 == 0x40
    keyphase w = w .|. 0x20 == 0x20

-- Long Packet Payload

getVersionNegotiation :: Get LongPacketPayload
getVersionNegotiation = VersionNegotiation <$> getQUICVersion <*> getQUICVersions

getClientInitial :: Get LongPacketPayload
getClientInitial         = undefined

getServerStatelessRetry = undefined

getServerClearText = undefined

getClientClearText = undefined

getZeroRTTProtected     = undefined

getOneRTTProtectedKeyPhaseZero    = undefined

getOneRTTProtectedKeyPhaseOne     = undefined

getPublicReset = undefined

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
getMsg = getInt32be >>= getmsg'
  where
    getmsg' n = if n == 0
                  then LBS.toStrict <$> getRemainingLazyByteString
                  else getByteString $ fromIntegral n

getStreamFrame ctx f ss oo d = Stream  <$> getStreamId ss <*> getOffset oo <*> getStreamData d
  where
    getStreamData :: Bool -> Get ByteString
    getStreamData d = do
     n <- if d then I.getInt16 else return 0
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
    getNumBlock n = if n then Just <$> I.getInt8 else return Nothing
    getNTS :: Get Integer
    getNTS = fromIntegral <$> getWord8
    getLAck :: LAckSize -> Get PacketNumber
    getLAck lack =  fromIntegral <$> case lack of
                      LAck1Byte -> I.getInt8
                      LAck2Byte -> I.getInt16
                      LAck4Byte -> I.getInt32
                      LAck8Byte -> I.getInt64

-- | getAckDelay
-- TODO:  Time Format
-- ref :https://github.com/quicwg/base-drafts/issues/109
-- reference implementation :
-- https://gist.github.com/mikkelfj/64deb01b86d68f3d7aacff4b113c22d8
getAckDelay :: Get  AckTimeDelta
getAckDelay =  fromIntegral <$> getInt16be

-- | getAckBlockLength
getAckBlockLength :: AckBlockLengthSize -> Get Integer
getAckBlockLength abl = fromIntegral <$> case abl of
                        AckBlock1Byte -> I.getInt8
                        AckBlock2Byte -> I.getInt16
                        AckBlock4Byte -> I.getInt32
                        AckBlock6Byte -> I.getInt48

-- | getAckBlocks
getAckBlocks :: Int -> AckBlockLengthSize -> Get AckBlock
getAckBlocks nblock abl = getAckBlockLength abl >>= (\ first -> getAckBlock abl first)
  where
    getAckBlock abl first = do
      pn0   <-  getAckBlockLength abl
      rest  <- getRestBlock abl (pn0 - first)
      return $ AckBlock ( [pn0, pn0-1.. pn0 - first ] ++  rest )
      where
        getGap = fromIntegral <$> I.getInt8
        getRestBlock :: AckBlockLengthSize -> PacketNumber ->  Get [PacketNumber]
        getRestBlock abl pn
          | (pn <= 0) = return []
          | otherwise = do
          gap <- getGap
          len <- getAckBlockLength abl
          rest <- getRestBlock abl (pn - gap - len)
          let pn' = pn - len
              cont =  [pn',pn'-1..(pn'- len)]
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
    getDelta = fromIntegral <$> I.getInt8
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
