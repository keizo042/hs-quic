module Network.QUIC.Codec.Put
  where

import           Data.Binary.Put

import           Data.Bits
import           Data.Int
import           Data.Maybe

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Network.QUIC.Codec.Internal
import qualified Network.QUIC.Internal       as I
import           Network.QUIC.Types

runPutStrict :: Put -> ByteString
runPutStrict  = LBS.toStrict . runPut

putPacketNumber :: PacketNumberSize -> PacketNumber -> Put
putPacketNumber size pn = case size of
  PacketNumber1Byte -> putWord8 $ fromIntegral pn
  PacketNumber2Byte -> putInt16be $ fromIntegral pn
  PacketNumber4Byte -> putInt32be $ fromIntegral pn

putConnectionId :: ConnectionId -> Put
putConnectionId = putWord64be . fromIntegral

putStreamId :: StreamSize -> StreamId -> Put
putStreamId size sid = case size of
     Stream1Byte -> putInt8 $ fromIntegral sid
     Stream2Byte -> putInt16be $ fromIntegral sid
     Stream3Byte -> I.putInt24 $ fromIntegral sid
     Stream4Byte -> putInt32be $ fromIntegral sid

putOffset :: OffsetSize -> Offset -> Put
putOffset size offset = case size of
  Offset2Byte ->  putInt16be $ fromIntegral offset
  Offset4Byte ->  putInt32be $ fromIntegral offset
  Offset8Byte ->  putInt64be $ fromIntegral offset

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
putHeader ctx@(EncodeContext ps  _ _ _ _) hdr = case hdr of
  (LongHeader typ c pn v) -> putLongHeader c ps pn v
  (ShortHeader c pn)      -> putShortHeader c ps pn

putLongHeader c psize pn v = putWord8 w >> putConnectionId c >> putPacketNumber psize pn >> putQUICVersion v
  where
    w = 0x80

putShortHeader c psize pn = putWord8 w >> putConnectionIdMaybe >> putPacketNumber psize pn
  where
    c' =  isJust c
    keypahse = undefined
    w = 0x00 .|. if c' then 0x40 else 0x00 .|. keypahse
    putConnectionIdMaybe = if c' then putConnectionId $ fromJust c else return ()


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
putFrameType ft = putWord8 $ fromFrameType ft

putFrame :: EncodeContext -> Frame -> Put
putFrame ctx@(EncodeContext ps ss oo fin d) frame = case frame of
  Padding                 -> putFrameType PaddingType  >> putPaddingFrame
  (RstStream s err offset)                   -> putFrameType RstStreamType >> putRstStreamFrame ss s err oo offset
  (ConnectionClose err s) -> putFrameType ConnectionCloseType  >> putConnectionCloseFrame err s
  (Goaway latest unknown) -> putFrameType GoawayType   >> putGoawayFrame ss latest unknown
  (MaxData i)             -> putFrameType MaxDataType  >> putMaxDataFrame i
  (MaxStreamData s i)     -> putFrameType MaxStreamDataType >> putMaxStreamDataFrame ss s i
  (MaxStreamId s)         -> putFrameType MaxStreamIdType >> putMaxStreamIdFrame ss s
  Ping                    -> putFrameType PingType >> putPingFrame
  Blocked                 -> putFrameType BlockedType >>  putBlockedFrame
  (StreamBlocked s)       -> putFrameType StreamBlockedType >> putStreamBlockedFrame s
  StreamIdNeeded          -> putFrameType StreamIdNeededType >> putStreamIdNeededFrame
  (NewConnectionId i cid) -> putFrameType NewConnectionType >> putNewConnectionId i cid

  (Stream s o bs)                -> putFrameType styp >> putStreamFrame ss s oo o bs
    where
      styp = StreamType fin ss oo d

  (Ack lack delay blocks stamps) -> putFrameType acktyp >> putAckFrame ctx lack delay blocks stamps
    where
      acktyp = AckType exists (toLAckSize lack) ablsize
      (exists, ablsize) = let AckBlock blks = blocks
                              l = length blks
                              ablsize = toAckBlockLengthSize l
                          in ( l > 1, ablsize)


putPaddingFrame :: Put
putPaddingFrame = return ()

putRstStreamFrame :: StreamSize -> StreamId -> ErrorCode -> OffsetSize -> Offset -> Put
putRstStreamFrame ssize s e osize offset = putStreamId ssize s >> putErrorCode e >> putOffset osize offset

putMaxDataFrame :: Int64 -> Put
putMaxDataFrame i = putInt64be i

putGoawayFrame :: StreamSize -> StreamId -> StreamId -> Put
putGoawayFrame  size latest unknown = putStreamId size latest >> putStreamId size unknown

putMaxStreamDataFrame :: StreamSize -> StreamId -> Int64 -> Put
putMaxStreamDataFrame size s i = do
    putStreamId size s
    putInt64be i

putMaxStreamIdFrame :: StreamSize -> StreamId -> Put
putMaxStreamIdFrame size sid = putStreamId size sid

putPingFrame :: Put
putPingFrame = return ()

putBlockedFrame :: Put
putBlockedFrame = return ()

putStreamIdNeededFrame = return ()

putNewConnectionId :: Int -> ConnectionId -> Put
putNewConnectionId i cid= undefined

putStreamBlockedFrame s = undefined


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
    putLargestAcked lacked = undefined
    putAckTimeDelta delta = undefined
    putAckBlocks    blocks = undefined
    putAckTimeStamp ts = undefined

putStreamFrame :: StreamSize -> StreamId -> OffsetSize -> Offset -> ByteString -> Put
putStreamFrame ssize s osize o bs = putStreamId ssize s >> putOffset osize o >> putStreamData bs
  where
    putStreamData bs = putStreamDataLength (BS.length bs) >> putByteString bs
    putStreamDataLength 0 = return ()
    putStreamDataLength i = putInt8 $ fromIntegral i
