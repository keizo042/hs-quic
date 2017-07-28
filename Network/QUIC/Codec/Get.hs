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

getTimeStamp :: Get.Get AckTimeStamp
getTimeStamp = undefined

getQUICVersion :: Get.Get QUICVersion
getQUICVersion = undefined

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
