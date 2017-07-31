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

-- | getAckTimeStamps
getAckTimeStamps :: PacketNumber  -- Largest Acked
                -> Integer            -- Delta Largest Acked
                ->  Get.Get AckTimeStamp
getAckTimeStamps lack n = do
    delta0  <- getGap
    stamp0  <- getQUICTime
    rest    <- getAckTimeStamp (n - 1) lack
    let first = (lack - delta0, stamp0)
    return $ AckTimeStamp (first : rest)
  where
    getGap = fromIntegral <$> I.getInt8

    getAckTimeStamp :: PacketNumber -> Integer -> Get.Get [(PacketNumber, QUICTime)]
    getAckTimeStamp _ 0 = return []
    getAckTimeStamp lack n = do
      gap <- getGap
      diff <- getQUICTime
      rest <- getAckTimeStamp lack (n - 1)
      return ((lack - gap, diff) : rest)

-- | getAckBlocks
getAckBlocks :: Int -> AckBlockLengthSize -> Get.Get AckBlock
getAckBlocks nblock abl = getAckBlockLength abl >>= (\ l -> getAckBlock abl l)
  where
    getAckBlockLength :: AckBlockLengthSize -> Get.Get Integer
    getAckBlockLength abl = fromIntegral <$> case abl of
                            AckBlock1Byte -> I.getInt8
                            AckBlock2Byte -> I.getInt16
                            AckBlock4Byte -> I.getInt32
                            AckBlock6Byte -> I.getInt48
    getAckBlock abl n = do
      pn0   <-  getAckBlockLength abl
      rest  <- getRestBlock abl pn0 n
      return $ AckBlock ( pn0 : rest )
      where
        getRestBlock :: AckBlockLengthSize -> PacketNumber -> Integer ->  Get.Get [Integer]
        getRestBlock abl pn 0 = return []
        getRestBlock abl pn n = do
          len <- getAckBlockLength abl
          let pn' = pn + len
          rest <- getRestBlock abl pn' (n - 1)
          return (pn' : rest)


getQUICVersion :: Get.Get QUICVersion
getQUICVersion = fromIntegral <$> I.getInt32

getQUICTime :: Get.Get QUICTime
getQUICTime = QUICTime <$> Get.getInt16be

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
