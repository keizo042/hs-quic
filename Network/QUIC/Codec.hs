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
          in case (decodeShortHeader ctx bs') of
            (Right payload) -> Right $ ShortPacket hdr payload
            (Left e)        -> Left e
        (LongHeader _ _ _ _) -> let ctx = undefined
          in case (decodeLongPacketPayload ctx bs') of
            (Right (payload, bs'')) -> Right $ LongPacket hdr payload
            (Left e)                -> Left e

      decodeShortHeader :: DecodeContext -> ByteString -> QUICResult ShortPacketPayload
      decodeShortHeader = decodeFrames


decodeFrames :: DecodeContext -> ByteString -> QUICResult [Frame]
decodeFrames ctx bs = case (decodeFrame ctx bs) of
  Left e        -> Left e
  Right (f,bs') -> decodeFrames' ctx f bs
    where
      decodeFrames' ctx f bs = case (decodeFrames ctx bs) of
         Right fs -> Right (f : fs)
         Left e   -> Left e




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
        getShortHeader = Get.getWord8 >>= get'
          where
            getConnIdMaybe w = if hasConn w then Just <$> getConnectionId else return Nothing
            hasConn w = w .|. 0x40 == 0x40
            get' w = ShortHeader <$> getConnIdMaybe w <*> getPacketNumber (toPacketNumberSize w)

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

            getQUICVersions :: Get.Get [QUICVersion]
            getQUICVersions =  Get.isEmpty >>= \ b -> if b
                                                    then return []
                                                    else do
                                                      v <- getQUICVersion
                                                      vs <- getQUICVersions
                                                      return (v:vs)

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
              _                   -> undefined
            getOneRTTProtectedKeyPhaseZero    = undefined

         OneRTTProctectedKeyPhaseOneType -> decodeOneRTTPRotectedKeyPhaseOne bs
          where
            decodeOneRTTPRotectedKeyPhaseOne bs  = case (Get.runGetOrFail getOneRTTProtectedKeyPhaseOne $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> undefined
            getOneRTTProtectedKeyPhaseOne     = undefined

         PublicResetType                 -> decodePublicReset bs
          where
            decodePublicReset bs = case (Get.runGetOrFail getPublicReset $ LBS.fromStrict bs) of
              (Right (bs', _, f)) -> Right (f, LBS.toStrict bs')
              _                   -> undefined
            getPublicReset = undefined

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



-- | encode is a API to encode to Packet of QUIC.
encode :: EncodeContext -> Packet -> ByteString
encode ctx (LongPacket hdr payload) = encodeHeader hdr `BS.append` encodeLongPacketPayload ctx payload
encode ctx (ShortPacket hdr payload)  = encodeHeader hdr `BS.append` encodeFrames ctx payload


encodeFrames :: EncodeContext -> [Frame] -> ByteString
encodeFrames _ []       = BS.empty
encodeFrames ctx (f:fs) = (encodeFrame ctx f) `BS.append` encodeFrames ctx fs


encodeLongPacketPayload :: EncodeContext -> LongPacketPayload -> ByteString
encodeLongPacketPayload ctx payload = f ctx payload
  where
    f ctx p = case p of
       (VersionNegotiation v vs)   -> LBS.toStrict $ Put.runPut $ putQUICVersion v >> mapM_ putQUICVersion vs
       (ClientInitial bs)               -> undefined
       (ServerCleartext bs)             -> undefined
       (ServerStatelessRetry bs)        -> undefined
       (ClientCleartext bs)             -> undefined
       (ZeroRTTProtected bs)            -> undefined
       (OneRTTProtectedKeyPhaseZero bs) -> undefined
       (OneRTTProtectedKeyPhaseOne bs)  -> undefined




encodeHeader :: Header -> ByteString
encodeHeader (LongHeader typ c pn v) = LBS.toStrict $ Put.runPut $ putLongHeader
  where
    w = 0x80
    putLongHeader = do
      Put.putWord8 w
      putLongHeaderType typ
      putConnectionId c
      putPacketNumber pn
      putQUICVersion v

encodeHeader (ShortHeader c pn)   = LBS.toStrict $ Put.runPut putShortHeader
  where
    w = 0x00
    putShortHeader = do
      Put.putWord8 w
      if Maybe.isJust c then putConnectionId $ Maybe.fromJust c else return ()
      putPacketNumber pn

encodeFrame :: EncodeContext -> Frame -> ByteString
encodeFrame ctx f = choice f
  where
    choice f = case f of
      (Stream s o bs)                   -> encodeStreamFrame s o bs
        where
          encodeStreamFrame s o bs = LBS.toStrict $ Put.runPut (putStreamFrame s o bs)

          putStreamFrame s o bs = putStreamId s >> putOffset o >> putStreamData bs
          -- TODO: naming is adhoc.
          putStreamData bs = putStreamDataLength (BS.length bs) >> Put.putByteString bs
          putStreamDataLength 0 = return ()
          putStreamDataLength i = Put.putInt8 $ fromIntegral i

      (Ack lack delay blocks stamps) -> undefined
        where
          encodeAckFrame ctx  =  LBS.toStrict . Put.runPut $ putAckFrame
            where
              putAckFrame =  undefined

      -- TODO: implement other encoder for frame
      _                                 -> undefined
    encodePaddingFrame :: Put.Put
    encodePaddingFrame  = Put.putByteString $ BS.singleton 0x60

    encodeGoawayFrame latest unknown = Put.runPut $ putGoawayFrame latest unknown
      where
        putGoawayFrame  = undefined

    encodeMaxDataFrame i = Put.runPut $ putMaxDataFrame i
      where
        putMaxDataFrame i = undefined

    encodeMaxStreamDataFrame sid size = Put.runPut $ putMaxStreamDataFrame sid size
      where
        putMaxStreamDataFrame = undefined

    encodeMaxStreamIdFrame sid = Put.runPut $ putMaxStreamIdFrame sid
      where
        putMaxStreamIdFrame sid = putStreamId sid

    encodeConnectionCloseFrame (ConnectionClose ecode msg) = Put.runPut $ putConnectionCloseFrame ecode msg
      where
        putConnectionCloseFrame :: ErrorCode -> ByteString -> Put.Put
        putConnectionCloseFrame e bs = putErrorCode e >> Put.putInt16be ( fromIntegral $ BS.length bs) >> Put.putByteString bs
