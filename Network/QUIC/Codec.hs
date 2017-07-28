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
  Right (hdr, bs') -> checkHeader hdr
    where
      checkHeader hdr =  case hdr of
        (ShortHeader _ _)    -> let ctx = undefined
          in case (decodeWithShortHeader ctx bs') of
            (Right payload) -> Right $ ShortPacket hdr payload
            (Left e)        -> Left e
        (LongHeader _ _ _ _) -> let ctx = undefined
          in case (decodeLongPacketPayload ctx bs') of
            (Right (payload, bs'')) -> Right $ LongPacket hdr payload
            (Left e)                -> Left e

      decodeWithShortHeader :: DecodeContext -> ByteString -> QUICResult ShortPacketPayload
      decodeWithShortHeader ctx bs = decodeFrames ctx bs


decodeFrames :: DecodeContext -> ByteString -> QUICResult [Frame]
decodeFrames ctx bs = case (decodeFrame ctx bs) of
  Left e        -> Left e
  Right (f,bs') -> checkFrames ctx f bs
    where
      checkFrames ctx f bs = case (decodeFrames ctx bs) of
         Right fs -> Right (f : fs)
         Left e   -> Left e




decodeHeader :: ByteString -> QUICResult (Header, ByteString)
decodeHeader bs =  case (toHeaderType $ BS.head bs) of
   LongHeaderType  -> decodeLongHeader bs
   ShortHeaderType -> decodeShortHeader bs
  where
    decodeLongHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeLongHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
       Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
       _                    -> Left QUICInvalidPacketHeader
      where
        decode' :: Get.Get Header
        decode' = Get.getWord8 >>= (\w -> LongHeader <$> getHeaderType w
                                   <*> getConnectionId
                                   <*> getPacketNumber PacketNumber4Byte
                                   <*> getQUICVersion)
            where
              getHeaderType :: Word8 -> Get.Get LongHeaderType
              getHeaderType w = case (toLongHeaderType w) of
                (Just t) -> return t
                Nothing  -> fail "invalid long header type"

    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
        Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
        _                    -> Left QUICInvalidPacketHeader
      where
        decode' :: Get.Get Header
        decode' = Get.getWord8 >>= d
          where
            getConnIdMaybe w = if hasConn w then Just <$> getConnectionId else return Nothing
            hasConn w = w .|. 0x40 == 0x40
            d w = ShortHeader <$> getConnIdMaybe w
                              <*> getPacketNumber (toPacketNumberSize w)

-- | decodeLongPacketPayload that it decode Payload with LongHeader.
-- | it was chcked  payload type in Long Header prefix octet and parse rest of octet.
decodeLongPacketPayload :: DecodeContext -> ByteString -> QUICResult (LongPacketPayload, ByteString)
decodeLongPacketPayload ctx bs = case (decodeContextLongPacketContext ctx) of
       (Just c) -> choiceDecoder (longPacketContextLongHeaderType c)
       Nothing  -> Left QUICInvalidPacketHeader
     where
       choiceDecoder typ = case typ of
         VersionNegotiationType          -> decodeVersionNegotiation bs
          where
            decodeVersionNegotiation bs = case (Get.runGetOrFail  decode $ LBS.fromStrict bs) of
               (Right (bs', _, (v:vs))) -> Right (VersionNegotiation v vs, LBS.toStrict bs')
               (Left _) -> Left QUICInvalidVersionNegotiationPacket
            decode :: Get.Get [QUICVersion]
            decode = Get.isEmpty >>= (\b -> if b then return [] else (:) <$> getQUICVersion <*> decode)

         ClientInitialType               -> decodeClientInitial
          where
           decodeClientInitial  = case (Get.runGetOrFail decode $ LBS.fromStrict bs) of
                (Right (bs', _, d)) -> Right (d, LBS.toStrict bs')
                _                   -> Left QUICInternalError -- TODO: maybe there are proper error.
           decode         = undefined

         ServerStatelessRetryType        -> decodeServerStatelessRetry
          where
            decodeServerStatelessRetry  = undefined
            decode = undefined

         ServerCleartextType             -> decodeServerClearText
          where
            decodeServerClearText = undefined
            decode = undefined

         ClientCleartextType             -> decodeClearClearText
          where
            decodeClearClearText  = undefined
            decode = undefined

         ZeroRTTProtectedType            -> decodeZeroRTTProtected
          where
            decodeZeroRTTProtected  = undefined
            decode = undefined

         OneRTTProtectedKeyPhaseZeroType -> decodeOneRTTProtectedKeyPhaseZero
          where
            decodeOneRTTProtectedKeyPhaseZero = undefined
            decode = undefined

         OneRTTProctectedKeyPhaseOneType -> decodeOneRTTPRotectedKeyPhaseOne
          where
            decodeOneRTTPRotectedKeyPhaseOne  = undefined
            decode = undefined

         PublicResetType                 -> decodePublicReset
          where
            decodePublicReset = undefined
            decode = undefined

decodeFrame :: DecodeContext -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame ctx bss = checkFrameType b
    where
     (b, bs) = (BS.head bss, BS.tail bss)


     checkFrameType :: Word8 -> QUICResult (Frame, ByteString)
     checkFrameType b = case (toFrameType b) of
        Nothing -> Left QUICInvalidFrameData
        Just ft -> choiceDecoder ft

     choiceDecoder ft = case ft of
       MaxDataType            -> decodeMaxDataFrame bs
       PaddingType            -> decodePaddingFrame bs
       PingType               -> decodePingFrame    bs

       MaxStreamDataType      -> decodeMaxStreamDataFrame   ctx bs
       MaxStreamIdType        -> decodeMaxStreamIdFrame     ctx bs
       StreamBlockedType      -> decodeStreamBlockedFrame   ctx bs
       StreamIdNeededType     -> decodeStreamIdNeededFrame  ctx bs
       RstStreamType          -> decodeRstStreamFrame       ctx bs
       NewConnectionType      -> decodeNewConnectionIdFrame ctx bs
       ConnectionCloseType    -> decodeConnectionCloseFrame ctx bs


       (StreamType f ss oo d) -> decodeStreamFrame ctx f ss oo d bs
       (AckType n lack abl)   -> decodeAckFrame ctx n lack abl bs


     decodeStreamFrame :: DecodeContext ->
                          Bool ->       -- stream is finished
                          StreamSize -> -- has stream id field
                          OffsetSize -> -- has offset field
                          Bool ->       -- has body filed
                          ByteString -> -- payload body
                          QUICResult (Frame, ByteString)  -- a frame and rest payload or error code
     decodeStreamFrame  ctx f ss oo d bs = case (Get.runGetOrFail decode  $ LBS.fromStrict bs) of
                                       (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                       _ -> Left QUICInvalidStreamData

        where
           decode :: Get.Get Frame
           decode = Stream  <$> getStreamId ss
                            <*> getOffset oo
                            <*> getStreamData d
           getStreamData :: Bool -> Get.Get ByteString
           getStreamData d = do
             n <- if d then I.getInt16 else return 0
             if n == 0 then return BS.empty else Get.getByteString $ fromIntegral n

     decodeAckFrame ctx n lack abl bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidAckData
        where
          decode' :: Get.Get Frame
          decode' =  do
            nblock <- getNumBlock
            nts   <- getNTS
            error "not yet implemented ack frame decoder"
          getNumBlock = if n then Just <$> I.getInt8 else return Nothing
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

     decodeMaxDataFrame  = f
        where
          f bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInvalidAckData
          decode' :: Get.Get Frame
          decode' = MaxData <$> Get.getInt64be

     decodeMaxStreamDataFrame = f
      where
          f ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
           (Right (rest, _, frm)) -> Right (frm, LBS.toStrict rest)
           _                      -> Left QUICInternalError

          ss = undefined
          decode' :: Get.Get Frame
          decode' = MaxStreamData <$> getStreamId ss <*> getMaxStreamData
          getMaxStreamData = fromIntegral <$> Get.getInt64be

     decodeMaxStreamIdFrame = f
       where
          f ctx bs =  case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInternalError

          ss = undefined
          decode' :: Get.Get Frame
          decode' = MaxStreamId <$> getStreamId ss

     decodeBlockedFrame bs  = Right (Blocked, bs)
     decodePaddingFrame bs  = Right (Padding, bs)
     decodePingFrame    bs  = Right (Ping,    bs)

     decodeStreamBlockedFrame ctx bs = f ctx bs
      where
        f ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
         (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
         _                    -> Left QUICInternalError

        ss = undefined
        decode' :: Get.Get Frame
        decode' = StreamBlocked <$> getStreamId ss

     decodeStreamIdNeededFrame ctx bs = f ctx bs
      where
          f ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInvalidStreamId

          decode' :: Get.Get Frame
          decode' = return StreamIdNeeded

     decodeRstStreamFrame ctx bs = f ctx bs
      where
        f ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
         (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
         _                    -> Left QUICInvalidRstStreamData

        ss = decodeContextStreamSize ctx
        oo = decodeContextOffsetSize ctx
        decode' :: Get.Get Frame
        decode' = RstStream <$> getStreamId ss <*> getErrorCode <*> getOffset oo

     decodeNewConnectionIdFrame ctx bs = f ctx  bs
      where
        f ctx bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
          (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
          _                    -> Left QUICInternalError

        decode' :: Get.Get Frame
        decode' = NewConnectionId <$> (fromIntegral <$> Get.getInt16be) <*> getConnectionId

     decodeConnectionCloseFrame ctx bs = f ctx bs
      where
          f ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInvalidConnectionCloseData

          decode' :: Get.Get Frame
          decode' = ConnectionClose <$> getErrorCode  <*> getMsg
          getMsg = Get.getInt32be >>= getmsg'
            where
              getmsg' n = if n == 0
                            then LBS.toStrict <$> Get.getRemainingLazyByteString
                            else Get.getByteString $ fromIntegral n

     decodeGoawayFrame ctx bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidGoawayData
        where
          ss = undefined
          decode' ::  Get.Get Frame
          decode' = Goaway <$> getStreamId ss <*> getStreamId ss



-- | encode is a API to encode to Packet of QUIC.
encode :: Packet -> ByteString
encode (LongPacket hdr payload) = encodeHeader hdr `BS.append` encodeLongPacketPayload ctx payload
   where
     ctx :: EncodeContext
     ctx = undefined
encode (ShortPacket hdr payload)  = encodeHeader hdr `BS.append` encodeFrames ctx payload
   where
     ctx :: EncodeContext
     ctx = undefined


encodeFrames :: EncodeContext -> [Frame] -> ByteString
encodeFrames _ []       = BS.empty
encodeFrames ctx (f:fs) = (encodeFrame ctx f) `BS.append` encodeFrames ctx fs


encodeLongPacketPayload :: EncodeContext -> LongPacketPayload -> ByteString
encodeLongPacketPayload ctx payload = f ctx payload
  where
    f ctx p = case p of
       (VersionNegotiation v vs)   -> LBS.toStrict $ Put.runPut $ putQUICVersion v >> mapM_ putQUICVersion vs
       ClientInitial               -> undefined
       ServerCleartext             -> undefined
       ServerStatelessRetry        -> undefined
       ClientCleartext             -> undefined
       ZeroRTTProtected            -> undefined
       OneRTTProtectedKeyPhaseZero -> undefined
       OneRTTProtectedKeyPhaseOne  -> undefined




encodeHeader :: Header -> ByteString
encodeHeader (LongHeader typ c pn v) = LBS.toStrict $ Put.runPut $ p
  where
    p = do
      putLongHeaderType typ
      putConnectionId c
      putPacketNumber pn
      putQUICVersion v

encodeHeader (ShortHeader c pn)   = LBS.toStrict $ Put.runPut p
  where
    p = do
      if Maybe.isJust c then putConnectionId $ Maybe.fromJust c else return ()
      putPacketNumber pn

encodeFrame :: EncodeContext -> Frame -> ByteString
encodeFrame ctx f = choice f
  where
    choice f = case f of
      (Stream s o bs)                   -> encodeStreamFrame s o bs
        where
          encodeStreamFrame s o bs = LBS.toStrict $ Put.runPut (p s o bs)
            where
              p s o bs = putStreamId s >> putOffset o >> putBS (BS.length bs)  >> Put.putByteString bs
              -- TODO: naming is adhoc.
              putBS 0 = return ()
              putBS i =  Put.putInt8 $ fromIntegral i

      (Ack mi i pktn t0 t1 ablk stamps) -> undefined
        where
          encodeAckFrame mi i pn t0 t1 ablk stamps      =  LBS.toStrict . Put.runPut $ p pn
            where
              p pn =  putPacketNumber pn

      -- TODO: implement other encoder for frame
      _                                 -> undefined
    encodePaddingFrame :: Put.Put
    encodePaddingFrame  = Put.putByteString $ BS.singleton 0x60
    encodeGoawayFrame   = undefined
    encoeMaxData        = undefined
    encodeMaxStreamData = undefined
    encodeMaxStreamId   = undefined
