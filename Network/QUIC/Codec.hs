module Network.QUIC.Codec
  (
  encode,
  decode
  )
  where

import qualified Data.Binary.Get             as Get
import           Data.Binary.Put             (runPut)
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
import           Network.QUIC.Time
import           Network.QUIC.Types


-- | decode is a API to decode Packet of QUIC.
decode :: ByteString -> QUICResult Packet
decode bs = decodeHeader bs >>= \ (hdr, bs') -> decodePayload hdr bs'
    where
      decodePayload hdr bs =  case hdr of
        (ShortHeader _ _)    -> decodeShortPacket (toDecodeContext hdr) hdr bs
        (LongHeader _ _ _ _) -> decodeLongPacket (toDecodeContext hdr)  hdr bs

      decodeShortPacket :: DecodeContext -> Header -> ByteString -> QUICResult Packet
      decodeShortPacket ctx hdr bs = case (decodeFrames ctx bs) of
                                       (Right p) -> Right $ ShortPacket hdr p
                                       (Left e)  -> Left e
      decodeLongPacket :: DecodeContext -> Header -> ByteString -> QUICResult Packet
      decodeLongPacket ctx hdr bs = case (decodeLongPacketPayload ctx bs) of
            (Right (payload, bs')) -> Right $ LongPacket hdr payload
            (Left e)               -> Left e


-- | decodeHeader
decodeHeader :: ByteString -> QUICResult (Header, ByteString)
decodeHeader bs =  case (toHeaderType $ BS.head bs) of
   LongHeaderType  -> decodeLongHeader bs
   ShortHeaderType -> decodeShortHeader bs
  where
    decodeLongHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeLongHeader bs = case (Get.runGetOrFail getLongHeader $ LBS.fromStrict bs) of
       Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
       _                    -> Left QUICInvalidPacketHeader
      where
        getLongHeader :: Get.Get Header
        getLongHeader = Get.getWord8 >>= (\w -> LongHeader <$> getHeaderType w
                                   <*> getConnectionId
                                   <*> getPacketNumber PacketNumber4Byte
                                   <*> getQUICVersion)
            where
              getHeaderType :: Word8 -> Get.Get LongHeaderType
              getHeaderType w = case (toLongHeaderType w) of
                (Just t) -> return t
                Nothing  -> fail "invalid long header type"

    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail getShortHeader $ LBS.fromStrict bs) of
        Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
        _                    -> Left QUICInvalidPacketHeader
      where
        getShortHeader :: Get.Get Header
        getShortHeader = Get.getWord8 >>= getShortHeader'
          where
            getConnIdMaybe w = if hasConn w then Just <$> getConnectionId else return Nothing
            hasConn w = w .|. 0x40 == 0x40
            getShortHeader' w = ShortHeader <$> getConnIdMaybe w <*> getPacketNumber (toPacketNumberSize w)

-- | decodeLongPacketPayload that it decode Payload with LongHeader.
-- | it was chcked  payload type in Long Header prefix octet and parse rest of octet.
decodeLongPacketPayload :: DecodeContext -> ByteString -> QUICResult (LongPacketPayload, ByteString)
decodeLongPacketPayload ctx bs = case (decodeContextLongPacketContext ctx) of
       (Just c) -> choiceDecoder (longPacketContextLongHeaderType c)
       Nothing  -> Left QUICInvalidPacketHeader
     where
       choiceDecoder typ = case typ of
         VersionNegotiationType -> decodeVersionNegotiation bs
          where
            decodeVersionNegotiation bs = case (Get.runGetOrFail getVersionNegotiation $ LBS.fromStrict bs) of
               (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
               (Left _) -> Left QUICInvalidVersionNegotiationPacket
            getVersionNegotiation :: Get.Get LongPacketPayload
            getVersionNegotiation = VersionNegotiation <$> getQUICVersion <*> getQUICVersions


         ClientInitialType  -> decodeClientInitial bs
          where
           decodeClientInitial  bs = case (Get.runGetOrFail getClientInitial $ LBS.fromStrict bs) of
                (Right (bs', _, d)) -> Right (d, LBS.toStrict bs')
                _                   -> Left QUICInternalError -- TODO: maybe there are proper error.
           getClientInitial :: Get.Get LongPacketPayload
           getClientInitial         = undefined

         ServerStatelessRetryType -> decodeServerStatelessRetry bs
          where
            decodeServerStatelessRetry bs= case (Get.runGetOrFail getServerStatelessRetry $ LBS.fromStrict bs) of
               (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
               _                   -> Left QUICInternalError -- TODO: maybe
            getServerStatelessRetry = undefined

         ServerCleartextType  -> decodeServerClearText bs
          where
            decodeServerClearText bs = case (Get.runGetOrFail getServerClearText $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> Left QUICInternalError
            getServerClearText = undefined

         ClientCleartextType  -> decodeClientClearText bs
          where
            decodeClientClearText bs =  case (Get.runGetOrFail getClientClearText $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> Left QUICInternalError

            getClientClearText = undefined

         ZeroRTTProtectedType            -> decodeZeroRTTProtected bs
          where
            decodeZeroRTTProtected bs = case (Get.runGetOrFail getZeroRTTProtected $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> Left QUICInternalError
            getZeroRTTProtected     = undefined

         OneRTTProtectedKeyPhaseZeroType -> decodeOneRTTProtectedKeyPhaseZero bs
          where
            decodeOneRTTProtectedKeyPhaseZero bs = case (Get.runGetOrFail getOneRTTProtectedKeyPhaseZero $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> Left QUICInternalError
            getOneRTTProtectedKeyPhaseZero    = undefined

         OneRTTProctectedKeyPhaseOneType -> decodeOneRTTPRotectedKeyPhaseOne bs
          where
            decodeOneRTTPRotectedKeyPhaseOne bs  = case (Get.runGetOrFail getOneRTTProtectedKeyPhaseOne $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> Left QUICInternalError
            getOneRTTProtectedKeyPhaseOne     = undefined

         PublicResetType                 -> decodePublicReset bs
          where
            decodePublicReset bs = case (Get.runGetOrFail getPublicReset $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> Left QUICInvalidPublicReset
            getPublicReset = undefined

-- | decodeFrames
decodeFrames :: DecodeContext -> ByteString -> QUICResult [Frame]
decodeFrames ctx bs = case (decodeFrame ctx bs) of
  Left e        -> Left e
  Right (f,bs') -> decodeFrames' ctx f bs
    where
      decodeFrames' ctx f bs = case (decodeFrames ctx bs) of
         Right fs -> Right (f : fs)
         Left e   -> Left e

-- | decodeFrame
decodeFrame :: DecodeContext -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame ctx bss = decodeFrameType b >>= \ ft -> decodeFrame0 ctx ft bs
    where
     (b, bs) = (BS.head bss, BS.tail bss)

     decodeFrameType :: Word8 -> QUICResult FrameType
     decodeFrameType b = case (toFrameType b) of
        Nothing -> Left QUICInvalidFrameData
        Just ft -> Right ft

     decodeFrame0 :: DecodeContext -> FrameType ->  ByteString -> QUICResult (Frame, ByteString)
     decodeFrame0 ctx ft bs  = case ft of
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
     decodeStreamFrame  ctx f ss oo d bs = case (Get.runGetOrFail (getStreamFrame ctx f ss oo d )  $ LBS.fromStrict bs) of
         (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
         _                    -> Left QUICInvalidStreamData

     decodeAckFrame ctx n lack abl bs = case (Get.runGetOrFail (getAckFrame n lack abl) $ LBS.fromStrict bs) of
         (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
         _                    -> Left QUICInvalidAckData

     decodeMaxDataFrame  = f
        where
          f bs = case (Get.runGetOrFail getMaxDataFrame $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInvalidAckData
          getMaxDataFrame :: Get.Get Frame
          getMaxDataFrame = MaxData <$> Get.getInt64be

     decodeMaxStreamDataFrame = f
      where
          f ctx bs = case (Get.runGetOrFail getMaxStreamDataFrame $ LBS.fromStrict bs) of
           (Right (rest, _, frm)) -> Right (frm, LBS.toStrict rest)
           _                      -> Left QUICInternalError

          ss = decodeContextStreamSize ctx
          getMaxStreamDataFrame :: Get.Get Frame
          getMaxStreamDataFrame = MaxStreamData <$> getStreamId ss <*> getMaxStreamData
          getMaxStreamData = fromIntegral <$> Get.getInt64be

     decodeMaxStreamIdFrame = f
       where
          f ctx bs =  case (Get.runGetOrFail getMaxStreamDataFrame $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInternalError

          ss = decodeContextStreamSize ctx
          getMaxStreamDataFrame :: Get.Get Frame
          getMaxStreamDataFrame = MaxStreamId <$> getStreamId ss

     decodeBlockedFrame bs  = Right (Blocked, bs)
     decodePaddingFrame bs  = Right (Padding, bs)
     decodePingFrame    bs  = Right (Ping,    bs)

     decodeStreamBlockedFrame ctx bs = f ctx bs
      where
        f ctx bs = case (Get.runGetOrFail getStreamBlocked $ LBS.fromStrict bs) of
         (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
         _                    -> Left QUICInternalError

        ss = decodeContextStreamSize ctx
        getStreamBlocked :: Get.Get Frame
        getStreamBlocked = StreamBlocked <$> getStreamId ss

     decodeStreamIdNeededFrame ctx bs = Right (StreamIdNeeded, bs)

     decodeRstStreamFrame ctx bs = f ctx bs
      where
        f ctx bs = case (Get.runGetOrFail getRstStreamFrame $ LBS.fromStrict bs) of
         (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
         _                    -> Left QUICInvalidRstStreamData

        ss = decodeContextStreamSize ctx
        oo = decodeContextOffsetSize ctx
        getRstStreamFrame :: Get.Get Frame
        getRstStreamFrame = RstStream <$> getStreamId ss <*> getErrorCode <*> getOffset oo

     decodeNewConnectionIdFrame ctx bs = f ctx  bs
      where
        f ctx bs= case (Get.runGetOrFail getNewConnectionId $ LBS.fromStrict bs) of
          (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
          _                    -> Left QUICInternalError

        getNewConnectionId :: Get.Get Frame
        getNewConnectionId = NewConnectionId <$> (fromIntegral <$> Get.getInt16be) <*> getConnectionId

     decodeConnectionCloseFrame = f
      where
          f ctx bs = case (Get.runGetOrFail getConnectionCloseFrame $ LBS.fromStrict bs) of
           (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
           _                    -> Left QUICInvalidConnectionCloseData

          getConnectionCloseFrame :: Get.Get Frame
          getConnectionCloseFrame = ConnectionClose <$> getErrorCode  <*> getMsg
          getMsg = Get.getInt32be >>= getmsg'
            where
              getmsg' n = if n == 0
                            then LBS.toStrict <$> Get.getRemainingLazyByteString
                            else Get.getByteString $ fromIntegral n

     decodeGoawayFrame ctx bs = case (Get.runGetOrFail getGoawayFrame $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidGoawayData
        where
          ss = decodeContextStreamSize ctx
          getGoawayFrame ::  Get.Get Frame
          getGoawayFrame = Goaway <$> getStreamId ss <*> getStreamId ss


--
--
-- encode
--
--

-- | encode is a API to encode to Packet of QUIC.
encode :: EncodeContext -> Packet -> ByteString
encode ctx (LongPacket hdr payload) = encodeHeader ctx hdr `BS.append` encodeLongPacketPayload ctx payload
encode ctx (ShortPacket hdr payload)  = encodeHeader ctx hdr `BS.append` encodeFrames ctx payload

-- | encodeLongPacketPayload
encodeLongPacketPayload :: EncodeContext -> LongPacketPayload -> ByteString
encodeLongPacketPayload ctx p = LBS.toStrict . runPut $ putLongPackerPaload ctx p

-- | encodeHeader
encodeHeader :: EncodeContext -> Header -> ByteString
encodeHeader ctx hdr = LBS.toStrict . runPut $ putHeader ctx hdr

-- | encodeFrames
encodeFrames :: EncodeContext -> [Frame] -> ByteString
encodeFrames _ []       = BS.empty
encodeFrames ctx (f:fs) = (encodeFrame ctx f) `BS.append` encodeFrames ctx fs

-- | encodeFrame
encodeFrame :: EncodeContext -> Frame -> ByteString
encodeFrame ctx frame = LBS.toStrict $ runPut (putFrame ctx frame)
