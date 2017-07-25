module Network.QUIC.Codec
  (
  encode,
  decode
  )
  where

import qualified Data.Binary.Get       as Get
import qualified Data.Binary.Put       as Put
import           Data.Bits
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int
import           Data.Maybe            (Maybe)
import qualified Data.Maybe            as Maybe
import           Data.Word             (Word8)
import           Network.QUIC
import qualified Network.QUIC.Internal as I
import           Network.QUIC.Types

-- | decode is a API to decode Packet of QUIC.
decode :: ByteString -> QUICResult Packet
decode bs = case (decodeHeader bs) of
              Left e           -> Left e
              Right (hdr, bs') -> case hdr of
                                    (ShortHeader _ _)    -> decodeShort hdr bs'
                                    (LongHeader _ _ _ _) -> case (decodeLongHeaderPacket bs') of
                                                              (Right (pkt, bs'')) -> decodeLong hdr pkt bs''
                                                              (Left e) -> Left e
    where
      decodeShort hdr bs = case (decodeFrames hdr bs) of
                (Right fs) -> Right (ShortPacket hdr fs)
                (Left e)   -> Left e
      decodeLong hdr pkt bs = case (decodeFrames hdr bs) of
                                (Right fs) -> Right (LongPacket  hdr pkt fs)
                                (Left e)   -> Left e

decodeLongHeaderPacket :: ByteString -> QUICResult (LongHeaderPacket, ByteString)
decodeLongHeaderPacket bs = undefined

-- | encode is a API to encode to Packet of QUIC.
encode :: Packet -> ByteString
encode (LongPacket hdr lp payload)       = encodeHeader hdr `BS.append`
                                           encodeLongHeaderPacket lp `BS.append`
                                           encodeFrames ctx payload
                                           where
                                             ctx = undefined
encode (ShortPacket hdr payload)         = encodeHeader hdr `BS.append`
                                           encodeFrames ctx payload
                                           where
                                             ctx = undefined

decodeFrames :: Header -> ByteString -> QUICResult [Frame]
decodeFrames hdr bs = case (decodeFrame hdr bs) of
                        Right (f,bs') -> case (decodeFrames hdr bs') of
                                     Right fs -> Right (f : fs)
                                     Left e   -> Left e
                        Left e  -> Left e

encodeFrames :: PacketContext -> [Frame] -> ByteString
encodeFrames _ []       = BS.empty
encodeFrames ctx (f:fs) = (encodeFrame ctx f) `BS.append` encodeFrames ctx fs

decodeHeader :: ByteString -> QUICResult (Header, ByteString)
decodeHeader bs =  case (toHeaderType $ BS.head bs) of
                     LongHeaderType  -> decodeLongHeader bs
                     ShortHeaderType -> decodeShortHeader bs
  where
    decodeLongHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeLongHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                         Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                         Left _               -> Left QUICInvalidPacketHeader
      where
        decode' :: Get.Get Header
        decode' = do
          w <- Get.getWord8
          cid <- getConnectionId
          pktn <- getPacketNumber
          v <- getQUICVersion
          case (toLongHeaderType w) of
            Nothing  -> error "must return invalid header type digit"
            (Just t) -> return $ LongHeader t cid pktn v
    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                          Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                          Left _               -> Left QUICInvalidPacketHeader
      where
        decode' :: Get.Get Header
        decode' = do
          w <- Get.getWord8
          c <- if hasConn w
                 then Just <$> getConnectionId
                 else return Nothing
          cid <- getConnectionId
          return $ ShortHeader c cid
          where
            hasConn w = w .|. 0x40 == 0x40

encodeLongHeaderPacket :: LongHeaderPacket -> ByteString
encodeLongHeaderPacket p = undefined

decodeFrame :: Header -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame hdr bss = case (toFrameType b) of
                       Nothing    -> undefined
                       Just ft  -> case ft of

                         MaxDataType          -> decodeMaxDataFrame bs
                         PaddingType          -> decodePaddingFrame bs
                         PingType             -> decodePingFrame    bs

                         MaxStreamDataType    -> decodeMaxStreamDataFrame   ctx bs
                         MaxStreamIdType      -> decodeMaxStreamIdFrame     ctx bs
                         StreamBlockedType    -> decodeStreamBlockedFrame   ctx bs
                         StreamIdNeededType   -> decodeStreamIdNeededFrame  ctx bs
                         RstStreamType        -> decodeRstStreamFrame       ctx bs
                         NewConnectionType    -> decodeNewConnectionIdFrame ctx bs
                         ConnectionCloseType  -> decodeConnectionCloseFrame ctx bs


                         (StreamType f ss oo d)  -> decodeStreamFrame hdr f ss oo d bs
                         (AckType n lack abl)    -> decodeAckFrame hdr n lack abl bs
   where
     (b, bs) = (BS.head bss, BS.tail bss)
     ctx = error "consider how we build packet context"
     decodeStreamFrame :: Header ->
                          Bool -> -- stream is finished
                          StreamSize -> -- has stream id field
                          OffsetSize -> -- has offset field
                          Bool -> -- has body filed
                          ByteString -> -- payload body
                          QUICResult (Frame, ByteString)  -- a frame and rest payload or error code
     decodeStreamFrame  hdr f ss oo d bs = case (Get.runGetOrFail decode'  $ LBS.fromStrict bs) of
                                       (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                       _ -> Left QUICInvalidStreamData

        where
           decode' :: Get.Get Frame
           decode' = Stream <$> getStreamId ss <*> getOffset oo <*> ( len >>= getStreamData)

           getStreamData :: Int -> Get.Get ByteString
           getStreamData n = if f
                             then return BS.empty
                             else LBS.toStrict <$> (Get.getLazyByteString $ fromIntegral n)
           len = if d then I.getInt16 else return 0

     decodeAckFrame hdr n lack abl bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidAckData
        where
          decode' :: Get.Get Frame
          decode' =  do
            nblock <- getNumBlock
            nts   <- getNTS
            error "not yet implemented ack frame decoder"
          getNumBlock = if n
                          then Just <$> I.getInt8
                          else return Nothing
          getNTS = fromIntegral <$> Get.getWord8
          getLAck lack =  fromIntegral <$> case lack of
                            LAck1Byte -> I.getInt8
                            LAck2Byte -> I.getInt16
                            LAck4Byte -> I.getInt32
                            LAck8Byte -> I.getInt64
          getAckBlock nblock = do
              l <- getAckBlockLength
              g l
              where
                g n = undefined
          getAckBlockLength = case abl of
                                  AckBlock1Byte -> undefined

          getTimeStamps :: Int -> Get.Get AckTimeStamp
          getTimeStamps nts = undefined

     decodeMaxDataFrame bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidAckData
        where
          decode' :: Get.Get Frame
          decode' = MaxData <$> Get.getInt64be

     decodeMaxStreamDataFrame ctx bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          ss = contextStreamSize ctx
          decode' :: Get.Get Frame
          decode' = MaxStreamData <$> getStreamId ss <*> getMaxStreamData
          getMaxStreamData = fromIntegral <$> Get.getInt64be

     decodeMaxStreamIdFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          ss = contextStreamSize ctx
          decode' :: Get.Get Frame
          decode' = MaxStreamId <$> getStreamId ss

     decodeBlockedFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidBlockedData
        where
          decode' :: Get.Get Frame
          decode' = return Blocked

     decodeStreamBlockedFrame ctx bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          ss = contextStreamSize ctx
          decode' :: Get.Get Frame
          decode' = StreamBlocked <$> getStreamId ss

     decodeStreamIdNeededFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidStreamId
        where
          decode' :: Get.Get Frame
          decode' = return StreamIdNeeded

     decodeRstStreamFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidRstStreamData
        where
          ss = contextStreamSize ctx
          oo = contextOffsetSize ctx
          decode' :: Get.Get Frame
          decode' = RstStream <$> getStreamId ss <*> getErrorCode <*> getOffset oo

     decodePaddingFrame bs = Right (Padding, bs)
     decodePingFrame bs = Right (Ping, BS.tail bs)
     decodeNewConnectionIdFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          decode' :: Get.Get Frame
          decode' = NewConnectionId <$> (fromIntegral <$> Get.getInt16be) <*> getConnectionId

     decodeConnectionCloseFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidConnectionCloseData
        where
          decode' :: Get.Get Frame
          decode' = ConnectionClose <$> getErrorCode  <*> getMsg
          getMsg = Get.getInt32be >>= (\ n -> if n == 0
                                  then LBS.toStrict <$> Get.getRemainingLazyByteString
                                  else Get.getByteString $ fromIntegral n)

     decodeGoawayFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidGoawayData
        where
          ss = contextStreamSize ctx
          decode' ::  Get.Get Frame
          decode' = Goaway <$> getStreamId ss <*> getStreamId ss



encodeHeader :: Header -> ByteString
encodeHeader (LongHeader typ c pn v) = LBS.toStrict $ Put.runPut $ do
                                  putLongHeaderType typ
                                  putConnectionId c
                                  putPacketNumber pn
                                  putQUICVersion v

encodeHeader (ShortHeader c pn)   = LBS.toStrict $ Put.runPut $ do
                                  if Maybe.isJust c
                                    then putConnectionId $ Maybe.fromJust c
                                    else return ()
                                  putPacketNumber pn

encodeFrame :: PacketContext -> Frame -> ByteString
encodeFrame ctx f =  case f of
                                  (Stream s o bs)    -> encodeStreamFrame s o bs
                                  (Ack mi i pktn t0 t1 ablk stamps) -> undefined
                                  _                           -> undefined
  where
    encodeStreamFrame s o bs = LBS.toStrict . Put.runPut $ putStreamId s >> putOffset o >> p (BS.length bs)  >> Put.putByteString bs
      where
        p 0 = return ()
        p i =  Put.putInt8 $ fromIntegral i
    encodeAckFrame mi i pn t0 t1 ablk stamps      =  LBS.toStrict . Put.runPut $ putPacketNumber pn
    encodePaddingFrame :: Put.Put
    encodePaddingFrame  = Put.putByteString $ BS.singleton 0x60
    encodeGoawayFrame   = undefined
    encoeMaxData        = undefined
    encodeMaxStreamData = undefined
    encodeMaxStreamId   = undefined


getPacketNumber :: Get.Get PacketNumber
getPacketNumber = undefined

putPacketNumber :: PacketNumber -> Put.Put
putPacketNumber = undefined

getConnectionId :: Get.Get ConnectionId
getConnectionId = fromIntegral <$> I.getInt64

putConnectionId :: ConnectionId -> Put.Put
putConnectionId = Put.putWord64be . fromIntegral

getStreamId :: StreamSize -> Get.Get StreamId
getStreamId Stream1Byte = I.getInt8
getStreamId Stream2Byte = I.getInt16
getStreamId Stream3Byte = I.getInt24
getStreamId Stream4Byte = I.getInt32

putStreamId :: StreamId -> Put.Put
putStreamId sid
  | (sid < 2^8 ) = Put.putWord8 $ fromIntegral sid
  | (sid < 2^16) = Put.putInt16be $ fromIntegral sid
  | (sid < 2^24) = error "not yet implemented in Network.QUIC.Codec"
  | otherwise = Put.putInt32be $ fromIntegral sid

getOffset :: OffsetSize -> Get.Get Offset
getOffset NoExistOffset = return 0
getOffset Offset2Byte   = fromIntegral <$> I.getInt16
getOffset Offset4Byte   = fromIntegral <$> I.getInt32
getOffset Offset8Byte   = fromIntegral <$> I.getInt64

putOffset :: Offset -> Put.Put
putOffset offset
  | (offset < 2^16) = Put.putInt16be $ fromIntegral offset
  | (offset < 2^32) = Put.putInt32be $ fromIntegral offset
  | otherwise       = Put.putInt64be $ fromIntegral offset

getTimeStamp :: Get.Get AckTimeStamp
getTimeStamp = undefined

getQUICVersion :: Get.Get QUICVersion
getQUICVersion = undefined

putQUICVersion :: QUICVersion ->  Put.Put
putQUICVersion v = undefined

-- | toHeaderType check Long or Short Header.
toHeaderType :: Word8 -> HeaderType
toHeaderType b
  | (b == 0x80) = LongHeaderType
  | otherwise =  ShortHeaderType

-- | fromHeaderType set Long/Short header filed of Word8.
fromHeaderType :: HeaderType -> Word8
fromHeaderType LongHeaderType  = 0x80
fromHeaderType ShortHeaderType = 0x00

-- | toLongHeaderType convert LongHeader type.
-- | if it is invaild, return Nothing.
toLongHeaderType :: Word8 -> Maybe LongHeaderType
toLongHeaderType w
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

-- | fromLongHeaderType set bit field of Word8.
fromLongHeaderType :: LongHeaderType -> Word8
fromLongHeaderType VersionNegotiationType          = 0x01
fromLongHeaderType ClientInitialType               = 0x02
fromLongHeaderType ServerStatelessRetryType        = 0x03
fromLongHeaderType ServerCleartextType             = 0x04
fromLongHeaderType ClientCleartextType             = 0x05
fromLongHeaderType ZeroRTTProtectedType            = 0x06
fromLongHeaderType OneRTTProtectedKeyPhaseZeroType = 0x07
fromLongHeaderType OneRTTProctectedKeyPhaseOneType = 0x08
fromLongHeaderType PublicResetType                 = 0x09

putLongHeaderType :: LongHeaderType -> Put.Put
putLongHeaderType t = Put.putWord8  (fromLongHeaderType t)

-- | hasConnectionId check existing ConnectionId Flag in Header.
hasConnectionId :: Word8 -> Bool
hasConnectionId w = w .&. 0x40 ==  0x40

-- | hasKeyPhase check existing Key Phase Flag in Header.
hasKeyPhase :: Word8 -> Bool
hasKeyPhase w = w .&. 0x20 == 0x20

-- | toFrameType detect short header type in Header.
toFrameType :: Word8 -> Maybe FrameType
toFrameType w = case (w .&. 0x1f) of
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
      -- 0xa0 - 0xbf
      -31  -> Just (StreamType (hasFin w) (chkStreamId w)  (chkOffset w)  (hasData w))
        where
           hasFin :: Word8 -> Bool
           hasFin w = w .&. 0x20 == 0x20

           chkStreamId :: Word8 -> StreamSize
           chkStreamId w = case (w .&. 0x18) of
                             0x00 -> Stream1Byte
                             0x08 -> Stream2Byte
                             0x10 -> Stream3Byte
                             0x18 -> Stream4Byte

           chkOffset :: Word8 -> OffsetSize
           chkOffset w = case (w .&. 0x06) of
                           0x00 -> NoExistOffset
                           0x02 -> Offset2Byte
                           0x06 -> Offset4Byte
                           0x08 -> Offset8Byte

           hasData :: Word8 -> Bool
           hasData w = w .&. 0x01 == 0x01
      -- 0xc0 - 0xff
      -63  -> Just (AckType (hasNumBlocksField w) (chkLACKField w) (chkAckBlockLengthField w))
        where
          hasNumBlocksField :: Word8 -> Bool
          hasNumBlocksField w = w .&. 0x10  == 0x10

          chkLACKField :: Word8 -> LAckSize
          chkLACKField w = case (w .&. 0xc0) of
                             0x00 -> LAck1Byte
                             0x40 -> LAck2Byte
                             0x80 -> LAck4Byte
                             0xc0 -> LAck8Byte

          chkAckBlockLengthField :: Word8 -> AckBlockLengthSize
          chkAckBlockLengthField w = case (w .&. 0x03) of
                                       0x00 -> AckBlock1Byte
                                       0x01 -> AckBlock2Byte
                                       0x02 -> AckBlock4Byte
                                       0x03 -> AckBlock8Byte

-- | fromFrameType
fromFrameType :: FrameType -> Word8
fromFrameType PaddingType                = 0x00
fromFrameType RstStreamType              = 0x01
fromFrameType ConnectionCloseType    = 0x03
fromFrameType GoawayType             = 0x04
fromFrameType MaxDataType            = 0x05
fromFrameType MaxStreamDataType      = 0x06
fromFrameType MaxStreamIdType        = 0x07
fromFrameType PingType               = 0x08
fromFrameType BlockedType            = 0x09
fromFrameType StreamBlockedType      = 0x0a
fromFrameType NewConnectionType      = 0x0b
fromFrameType (StreamType f ss oo d) = 0xa0 - 0xbf .|. fin f .|. stream ss .|. offset oo .|. adata d
  where
    fin True  = 0x20
    fin False =0x00
    stream Stream1Byte=0x00
    stream Stream2Byte=0x08
    stream Stream3Byte=0x10
    stream Stream4Byte=0x18

    offset NoExistOffset = 0x00
    offset Offset2Byte   = 0x02
    offset Offset4Byte   = 0x04
    offset Offset8Byte   = 0x06

    adata True  = 0x01
    adata False = 0x00
fromFrameType (AckType n ll mm)      =  0xc0 - 0xff .|. nblock n .|. len ll .|. ablk mm
  where
    nblock True  = 0x10
    nblock False = 0x00
    len LAck1Byte = 0x00
    len LAck2Byte = 0x40
    len LAck4Byte = 0x80
    len LAck8Byte = 0xc0
    ablk AckBlock1Byte = 0x00
    ablk AckBlock2Byte = 0x01
    ablk AckBlock4Byte = 0x02
    ablk AckBlock8Byte = 0x03
