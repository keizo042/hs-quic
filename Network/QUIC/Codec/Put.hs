module Network.QUIC.Codec.Put
  where

import           Data.Binary.Put
import           Data.Int
import           Data.Maybe

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Network.QUIC.Codec.Internal
import qualified Network.QUIC.Internal       as I
import           Network.QUIC.Types

putPacketNumber :: PacketNumber -> Put
putPacketNumber =  putInt64be . fromIntegral

putConnectionId :: ConnectionId -> Put
putConnectionId = putWord64be . fromIntegral

putStreamId :: StreamId -> Put
putStreamId sid
  | (sid < 2^8 ) = putWord8 $ fromIntegral sid
  | (sid < 2^16) = putInt16be $ fromIntegral sid
  | (sid < 2^24) = error "not yet implemented in Network.QUIC.Codec"
  | otherwise = putInt32be $ fromIntegral sid

putOffset :: Offset -> Put
putOffset offset
  | (offset < 2^16) = putInt16be $ fromIntegral offset
  | (offset < 2^32) = putInt32be $ fromIntegral offset
  | otherwise       = putInt64be $ fromIntegral offset

-- | putErrorCode
putErrorCode :: ErrorCode -> Put
putErrorCode e = putInt32be . fromIntegral $ errorCodeToInt e

putQUICVersion :: QUICVersion ->  Put
putQUICVersion v = putInt32be $ fromIntegral v


--
-- Header
--

putLongHeaderType :: LongHeaderType -> Put
putLongHeaderType t = putWord8  (fromLongHeaderType t)

putHeader :: EncodeContext -> Header -> Put
putHeader ctx hdr = case hdr of
                      (LongHeader typ c pn v) -> putLongHeader c pn v
                      (ShortHeader c pn)      -> putShortHeader c pn

putLongHeader c pn v = putWord8 0x80 >> putConnectionId c >> putPacketNumber pn >> putQUICVersion v

putShortHeader c pn = putWord8 0x00 >> putConnectionIdMaybe >> putPacketNumber pn
  where
    putConnectionIdMaybe = if isJust c then putConnectionId $ fromJust c else return ()


--
--
-- Long Packet Payload
--
--

putLongPackerPaload :: EncodeContext -> LongPacketPayload -> Put
putLongPackerPaload ctx p = case p of
   (VersionNegotiation v vs)        ->  putVersionNegotiation  v vs
   (ClientInitial bs)               ->  putByteString bs
   (ServerCleartext bs)             ->  putByteString bs
   (ServerStatelessRetry bs)        ->  putByteString bs
   (ClientCleartext bs)             ->  putByteString bs
   (ZeroRTTProtected bs)            ->  putByteString bs
   (OneRTTProtectedKeyPhaseZero bs) ->  putByteString bs
   (OneRTTProtectedKeyPhaseOne bs)  ->  putByteString bs
-- | putVersionNegotiation
putVersionNegotiation v vs =  putQUICVersion v >> mapM_ putQUICVersion vs

putConnectionCloseFrame :: ErrorCode -> ByteString -> Put
putConnectionCloseFrame e bs = putErrorCode e >> putInt16be ( fromIntegral $ BS.length bs) >> putByteString bs


--
-- Short Packet Payload  a.k.a Protected Payload
--

putFrameType :: FrameType -> Put
putFrameType ft = undefined

putFrame :: EncodeContext -> Frame -> Put
putFrame ctx frame = case frame of
  Padding                        -> putFrameType PaddingType  >> putPaddingFrame
  (Goaway latest unknown)        -> putFrameType GoawayType   >> putGoawayFrame latest unknown
  (MaxData i)                    -> putFrameType MaxDataType  >> putMaxDataFrame i
  (MaxStreamData s i)            -> putFrameType MaxStreamDataType    >>  putMaxStreamDataFrame s i
  (MaxStreamId s)                -> putFrameType MaxStreamIdType      >> putMaxStreamIdFrame s
  (ConnectionClose err s)        -> putFrameType ConnectionCloseType  >> putConnectionCloseFrame err s

  (Stream s o bs)                -> putFrameType styp >> putStreamFrame s o bs
    where
      styp = StreamType (encodeContextStreamFin ctx)
                        (encodeContextStreamSize ctx)
                        (encodeContextOffsetSize ctx)
                        (encodeContextStreamHasData ctx)

  (Ack lack delay blocks stamps) -> putFrameType acktyp >> putAckFrame ctx lack delay blocks stamps
    where
      toAckBlockLength = undefined
      toLAckSize = undefined
      acktyp = AckType exists lacksize ablsize
      lacksize = toLAckSize lack
      (exists, ablsize) = let AckBlock blks = blocks
                              l = length blks
                              ablsize = toAckBlockLength l
                          in if l > 1 then (True, ablsize) else (False, ablsize)


putPaddingFrame :: Put
putPaddingFrame = return ()

putMaxDataFrame :: Int64 -> Put
putMaxDataFrame i = putInt64be i

putGoawayFrame :: StreamId -> StreamId -> Put
putGoawayFrame  latest unknown = putStreamId latest >> putStreamId unknown

putMaxStreamDataFrame :: StreamId -> Int64 -> Put
putMaxStreamDataFrame s i = do
    putStreamId s
    putInt64be i

putMaxStreamIdFrame :: StreamId -> Put
putMaxStreamIdFrame sid = putStreamId sid

putStreamFrame :: StreamId -> Offset -> ByteString -> Put
putStreamFrame s o bs = putStreamId s >> putOffset o >> putStreamData bs
  where
    putStreamData bs = putStreamDataLength (BS.length bs) >> putByteString bs
    putStreamDataLength 0 = return ()
    putStreamDataLength i = putInt8 $ fromIntegral i

putAckFrame :: EncodeContext
            -> PacketNumber
            -> AckTimeDelta
            -> AckBlock
            -> AckTimeStamp
            -> Put
putAckFrame ctx lack delay blocks stamps =  do
    putLargestAcked lack
    putAckTimeDelta delay
    putAckBlocks blocks
    putAckTimeStamp stamps
  where
    putLargestAcked = undefined
    putAckTimeDelta = undefined
    putAckBlocks    = undefined
    putAckTimeStamp = undefined
