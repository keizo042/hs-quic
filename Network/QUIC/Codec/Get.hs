module Network.QUIC.Codec.Get
  where
import qualified Data.Binary.Get             as Get

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Data.Int

import           Network.QUIC.Codec.Internal
import qualified Network.QUIC.Internal       as I
import           Network.QUIC.Types
import qualified Network.QUIC.UFloat16



getQUICVersion :: Get.Get QUICVersion
getQUICVersion = fromIntegral <$> I.getInt32

getQUICVersions :: Get.Get [QUICVersion]
getQUICVersions =  Get.isEmpty >>= \ b -> if b then return [] else get
  where
     get = do
        v <- getQUICVersion
        vs <- getQUICVersions
        return (v:vs)

getQUICTime :: Get.Get QUICTime
getQUICTime = fromIntegral <$> Get.getInt32be

getOffset :: OffsetSize -> Get.Get Offset
getOffset NoExistOffset = return 0
getOffset Offset2Byte   = fromIntegral <$> I.getInt16
getOffset Offset4Byte   = fromIntegral <$> I.getInt32
getOffset Offset8Byte   = fromIntegral <$> I.getInt64

getStreamId :: StreamSize -> Get.Get StreamId
getStreamId Stream1Byte = I.getInt8
getStreamId Stream2Byte = I.getInt16
getStreamId Stream3Byte = I.getInt24
getStreamId Stream4Byte = I.getInt32

getConnectionId :: Get.Get ConnectionId
getConnectionId = fromIntegral <$> I.getInt64

getPacketNumber :: PacketNumberSize ->  Get.Get PacketNumber
getPacketNumber p = fromIntegral <$> case p of
  PacketNumber1Byte -> I.getInt8
  PacketNumber2Byte -> I.getInt16
  PacketNumber4Byte -> I.getInt32

-- | getErrorCode
getErrorCode :: Get.Get ErrorCode
getErrorCode = intToErrorCode . fromIntegral <$> I.getInt32



-- | Frame
-- | getStreamFrame
getStreamFrame ctx f ss oo d = Stream  <$> getStreamId ss <*> getOffset oo <*> getStreamData d
  where
    getStreamData :: Bool -> Get.Get ByteString
    getStreamData d = do
     n <- if d then I.getInt16 else return 0
     if n == 0 then return BS.empty else Get.getByteString $ fromIntegral n

-- | getAckFrame
getAckFrame  :: Bool -> LAckSize -> AckBlockLengthSize -> Get.Get Frame
getAckFrame n lack abl = getNumBlock n >>=
                        \ nblock ->  getNTS >>=
                        \ nstamps -> getLAck lack >>=
                        \ l ->
  Ack l <$> getAckDelay <*> getAckBlocksMaybe nblock abl <*> getAckTimeStamps l nstamps
  where
    getAckBlocksMaybe :: Maybe Int -> AckBlockLengthSize -> Get.Get AckBlock
    getAckBlocksMaybe Nothing abl        =  getAckBlocks 0 abl
    getAckBlocksMaybe (Just nblocks) abl =  getAckBlocks nblocks abl

    getNumBlock :: Bool -> Get.Get (Maybe Int)
    getNumBlock n = if n then Just <$> I.getInt8 else return Nothing
    getNTS :: Get.Get Integer
    getNTS = fromIntegral <$> Get.getWord8
    getLAck :: LAckSize -> Get.Get PacketNumber
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
getAckDelay :: Get.Get  AckTimeDelta
getAckDelay = parse <$> Get.getInt16be
  where
    parse = undefined

-- | getAckBlockLength
getAckBlockLength :: AckBlockLengthSize -> Get.Get Integer
getAckBlockLength abl = fromIntegral <$> case abl of
                        AckBlock1Byte -> I.getInt8
                        AckBlock2Byte -> I.getInt16
                        AckBlock4Byte -> I.getInt32
                        AckBlock6Byte -> I.getInt48

-- | getAckBlocks
getAckBlocks :: Int -> AckBlockLengthSize -> Get.Get AckBlock
getAckBlocks nblock abl = getAckBlockLength abl >>= (\ first -> getAckBlock abl first)
  where
    getAckBlock abl first = do
      pn0   <-  getAckBlockLength abl
      rest  <- getRestBlock abl (pn0 - first)
      return $ AckBlock ( [pn0, pn0-1.. pn0 - first ] ++  rest )
      where
        getGap = fromIntegral <$> I.getInt8
        getRestBlock :: AckBlockLengthSize -> PacketNumber ->  Get.Get [PacketNumber]
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
                ->  Get.Get AckTimeStamp
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

    getAckTimeStamp :: PacketNumber -> QUICTime -> Integer -> Get.Get [(PacketNumber, QUICTime)]
    getAckTimeStamp _     time 0 = return []
    getAckTimeStamp lack  time n = do
      gap <- getDelta
      diff <- getQUICTimeDelta
      let time' = addQUICTimeDelta time diff
      rest <- getAckTimeStamp lack time' (n - 1)
      return ((lack - gap, time') : rest)
