module Network.QUIC.Codec
  (
  encode,
  decode
  )
  where

import qualified Data.Binary.Get             as Get
import qualified Data.Binary.Put             as Put
import           Network.QUIC.Codec.Get
import           Network.QUIC.Codec.Internal
import           Network.QUIC.Codec.Put

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Data.Bits
import           Data.Int
import           Data.Maybe                  (Maybe)
import qualified Data.Maybe                  as Maybe
import           Data.Word                   (Word8)

import           Network.QUIC
import qualified Network.QUIC.Internal       as I
import           Network.QUIC.Types

-- | decode is a API to decode Packet of QUIC.
decode :: ByteString -> QUICResult Packet
decode bs = case (decodeHeader bs) of
              Left e           -> Left e
              Right (hdr, bs') -> case hdr of
                  (ShortHeader _ _)    -> decodeWithShort hdr bs'
                  (LongHeader _ _ _ _) -> case (decodeLongHeaderPayload bs') of
                        (Right (payload, bs'')) -> Right $ LongPacket hdr payload
                        (Left e)                -> Left e
    where
      decodeWithShort hdr bs = case (decodeFrames hdr bs) of
          (Right fs) -> Right (ShortPacket hdr fs)
          (Left e)   -> Left e


decodeFrames :: Header -> ByteString -> QUICResult [Frame]
decodeFrames hdr bs = case (decodeFrame hdr bs) of
                        Right (f,bs') -> case (decodeFrames hdr bs') of
                                     Right fs -> Right (f : fs)
                                     Left e   -> Left e
                        Left e  -> Left e



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
            Nothing  -> error "must return invalid header type digit" -- TODO: See error function
            (Just t) -> return $ LongHeader t cid pktn v
    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
        Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
        Left _               -> Left QUICInvalidPacketHeader
      where
        decode' :: Get.Get Header
        decode' = do
          w <- Get.getWord8
          c <- if hasConn w then Just <$> getConnectionId else return Nothing
          cid <- getConnectionId
          return $ ShortHeader c cid
          where
            hasConn w = w .|. 0x40 == 0x40

decodeLongHeaderPayload :: ByteString -> QUICResult (LongHeaderPayload, ByteString)
decodeLongHeaderPayload bs = undefined

decodeFrame :: Header -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame hdr bss = case (toFrameType b) of
                       Nothing    -> Left QUICInvalidFrameData
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
     -- TODO: desgin what packet context reuqire
     ctx = error "consider how we build packet context"

     decodeStreamFrame :: Header ->
                          Bool ->       -- stream is finished
                          StreamSize -> -- has stream id field
                          OffsetSize -> -- has offset field
                          Bool ->       -- has body filed
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
                             else Get.getByteString $ fromIntegral n
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

     decodeBlockedFrame bs  = Right (Blocked, bs)
     decodePaddingFrame bs  = Right (Padding, bs)
     decodePingFrame    bs  = Right (Ping,    bs)

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



-- | encode is a API to encode to Packet of QUIC.
encode :: Packet -> ByteString
encode (LongPacket hdr payload)       = encodeHeader hdr `BS.append`
                                           encodeLongHeaderPayload payload
                                           where
                                             ctx :: PacketContext
                                             ctx = undefined
encode (ShortPacket hdr payload)         = encodeHeader hdr `BS.append`
                                           encodeFrames ctx payload
                                           where
                                             ctx :: PacketContext
                                             ctx = undefined


encodeFrames :: PacketContext -> [Frame] -> ByteString
encodeFrames _ []       = BS.empty
encodeFrames ctx (f:fs) = (encodeFrame ctx f) `BS.append` encodeFrames ctx fs


encodeLongHeaderPayload :: LongHeaderPayload -> ByteString
encodeLongHeaderPayload (VersionNegotiation v vs)   = LBS.toStrict $ Put.runPut $ putQUICVersion v >> mapM_ putQUICVersion vs
encodeLongHeaderPayload ClientInitial               = undefined
encodeLongHeaderPayload ServerCleartext             = undefined
encodeLongHeaderPayload ServerStatelessRetry        = undefined
encodeLongHeaderPayload ClientCleartext             = undefined
encodeLongHeaderPayload ZeroRTTProtected            = undefined
encodeLongHeaderPayload OneRTTProtectedKeyPhaseZero = undefined
encodeLongHeaderPayload OneRTTProtectedKeyPhaseOne  = undefined




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
                                  -- TODO: implement other encoder for
                                  -- frame
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
