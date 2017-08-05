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
     Stream3Byte -> putInt24 $ fromIntegral sid
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

putQUICTime :: QUICTime -> Put
putQUICTime t = putInt32be $ fromIntegral t

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
   (VersionNegotiation v vs)        -> putVersionNegotiation  v vs
   (ClientInitial fs)               -> putFrames ctx fs
   (ServerCleartext fs)             -> putFrames ctx fs
   (ServerStatelessRetry fs)        -> putFrames ctx fs
   (ClientCleartext fs)             -> putFrames ctx fs
   (ZeroRTTProtected fs)            -> putFrames ctx fs
   (OneRTTProtectedKeyPhaseZero fs) -> putFrames ctx fs
   (OneRTTProtectedKeyPhaseOne fs)  -> putFrames ctx fs

-- | putVersionNegotiation
putVersionNegotiation v vs =  putQUICVersion v >> mapM_ putQUICVersion vs

putConnectionCloseFrame :: ErrorCode -> ByteString -> Put
putConnectionCloseFrame e bs = putErrorCode e >> putInt16be ( fromIntegral $ BS.length bs) >> putByteString bs

--
-- Short Packet Payload  a.k.a Protected Payload
--

putFrames :: EncodeContext -> [Frame] -> Put
putFrames ctx []     = return ()
putFrames ctx (x:xs) = putFrame ctx x >> putFrames ctx xs

putFrameType :: FrameType -> Put
putFrameType ft = putWord8 $ fromFrameType ft

putFrame :: EncodeContext -> Frame -> Put
putFrame ctx@(EncodeContext ps ss oo fin d) frame = case frame of
  Padding                 -> putFrameType PaddingType  >> putPaddingFrame
  (RstStream s err offset) -> putFrameType RstStreamType >> putRstStreamFrame s err offset
  (Goaway latest unknown) -> putFrameType GoawayType   >> putGoawayFrame latest unknown
  (MaxData i)             -> putFrameType MaxDataType  >> putMaxDataFrame i
  (MaxStreamData s i)     -> putFrameType MaxStreamDataType >> putMaxStreamDataFrame s i
  (MaxStreamId s)         -> putFrameType MaxStreamIdType >> putMaxStreamIdFrame s
  Ping                    -> putFrameType PingType >> putPingFrame
  Blocked                 -> putFrameType BlockedType >>  putBlockedFrame
  (StreamBlocked s)       -> putFrameType StreamBlockedType >> putStreamBlockedFrame s
  StreamIdNeeded          -> putFrameType StreamIdNeededType >> putStreamIdNeededFrame
  (NewConnectionId i cid) -> putFrameType NewConnectionType >> putNewConnectionId i cid
  (ConnectionClose err s) -> putFrameType ConnectionCloseType  >> putConnectionCloseFrame err s

  (Stream s o bs)                -> putFrameType styp >> putStreamFrame ss s oo o bs
    where
      styp = StreamType fin ss oo d

  (Ack lack delay blocks stamps) -> putFrameType acktyp >> putAckFrame ctx lsize lack delay blocks stamps
    where
      acktyp = AckType exists lsize ablsize
      lsize = toLAckSize lack
      (exists, ablsize) = let AckBlock blks = blocks
                              l = length blks
                              ablsize = toAckBlockLengthSize l
                          in ( l > 1, ablsize)


putPaddingFrame :: Put
putPaddingFrame = return ()

putRstStreamFrame ::  StreamId -> ErrorCode -> Offset -> Put
putRstStreamFrame s e offset = putStreamId Stream4Byte s >> putErrorCode e >> putOffset Offset4Byte offset

putMaxDataFrame :: Int64 -> Put
putMaxDataFrame i = putInt64be i

putGoawayFrame ::  StreamId -> StreamId -> Put
putGoawayFrame  latest unknown = putStreamId Stream4Byte latest >> putStreamId Stream4Byte unknown

putMaxStreamDataFrame ::  StreamId -> Int64 -> Put
putMaxStreamDataFrame s i = putStreamId Stream4Byte s >> putInt64be i

putMaxStreamIdFrame :: StreamId -> Put
putMaxStreamIdFrame sid = putStreamId Stream4Byte sid

putPingFrame :: Put
putPingFrame = return ()

putBlockedFrame :: Put
putBlockedFrame = return ()

putStreamIdNeededFrame = return ()

putNewConnectionId :: Int16 -> ConnectionId -> Put
putNewConnectionId i cid = putInt16be i >> putConnectionId cid

putStreamBlockedFrame :: StreamId -> Put
putStreamBlockedFrame s = putStreamId Stream4Byte s


putAckFrame :: EncodeContext
            -> LAckSize
            -> PacketNumber
            -> AckTimeDelta
            -> AckBlock
            -> AckTimeStamp
            -> Put
putAckFrame ctx lsize lacked delay blocks stamps =  do
    putLargestAcked lsize lacked
    putAckTimeDelta delay
    putAckBlocks blocks
    putAckTimeStamp lacked stamps
  where
    putLargestAcked :: LAckSize -> PacketNumber -> Put
    putLargestAcked lsize lacked = undefined

    putAckTimeDelta :: AckTimeDelta -> Put
    putAckTimeDelta delta = putInt16be $ fromIntegral delta

    putAckBlocks :: AckBlock -> Put
    putAckBlocks  (AckBlock blocks) = undefined

    putFirstTampStamp :: PacketNumber -> PacketNumber -> QUICTime -> Put
    putFirstTampStamp lacked pn time = (putInt8 $ fromIntegral (lacked - pn)) >> putQUICTime time

    putAckTimeStamp :: PacketNumber -> AckTimeStamp -> Put
    putAckTimeStamp lacked (AckTimeStamp [(pn,time)])  = putFirstTampStamp lacked pn time
    putAckTimeStamp lacked (AckTimeStamp ((pn,time):xs))  = do
      putFirstTampStamp lacked pn time
      putTimestamps lacked time xs
      where
        prevQUICTime :: QUICTime -> AckTimeDelta -> QUICTime
        prevQUICTime = undefined
        putGap :: Int8 -> Put
        putGap = putInt8
        putTimestamps :: PacketNumber -> QUICTime -> [(PacketNumber,QUICTime)] -> Put
        putTimestamps _ _ []             = return ()
        putTimestamps lacked time ((pn,t):xs) = do
          let delta  = (diffQUICTime time t)
          putGap $ fromIntegral $ lacked - pn
          putAckTimeDelta delta
          putTimestamps lacked (prevQUICTime time delta) xs


putStreamFrame :: StreamSize -> StreamId -> OffsetSize -> Offset -> ByteString -> Put
putStreamFrame ssize s osize o bs = putStreamId ssize s >> putOffset osize o >> putStreamData bs
  where
    putStreamData bs = putStreamDataLength (BS.length bs) >> putByteString bs
    putStreamDataLength 0 = return ()
    putStreamDataLength i = putInt8 $ fromIntegral i


--- Internal

putInt24 :: Int -> Put
putInt24 n = error "put utlitiy"
