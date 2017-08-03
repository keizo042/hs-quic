module Network.QUIC.Codec
  (
  encode,
  decode
  )
  where

import           Data.Binary                 (Get)
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
decodeHeader bs =  case (toHeaderType b) of
   LongHeaderType  -> case (toLongHeaderType b) of
                        (Just h) -> decodeLongHeader h bs
                        Nothing  -> Left QUICInvalidPacketHeader
   ShortHeaderType -> decodeShortHeader b bs
  where
    (b, bs') = (BS.head bs, BS.tail bs)

    runGetOrFail' :: Get Header -> ByteString -> QUICResult (Header, ByteString)
    runGetOrFail' f bs =  runGetOrFailWithError bs f QUICInvalidPacketHeader

    decodeLongHeader :: LongHeaderType -> ByteString -> QUICResult (Header, ByteString)
    decodeLongHeader h bs = runGetOrFail' (getLongHeader h) bs
    decodeShortHeader :: Word8 -> ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader b bs = runGetOrFail' (getShortHeader b) bs


-- | decodeLongPacketPayload that it decode Payload with LongHeader.
-- | it was chcked  payload type in Long Header prefix octet and parse rest of octet.
decodeLongPacketPayload :: DecodeContext -> ByteString -> QUICResult (LongPacketPayload, ByteString)
decodeLongPacketPayload ctx bs = case (decodeContextLongPacketContext ctx) of
       (Just c) -> run (longPacketContextLongHeaderType c) bs
       Nothing  -> Left QUICInvalidPacketHeader
     where
       run typ bs = case typ of
         VersionNegotiationType           -> runGetOrFailWithError bs getVersionNegotiation   QUICInvalidVersionNegotiationPacket
         ClientInitialType                -> runGetOrFailWithError bs getClientInitial        QUICInternalError
         ServerStatelessRetryType         -> runGetOrFailWithError bs getServerStatelessRetry QUICInternalError
         ServerCleartextType              -> runGetOrFailWithError bs getServerClearText      QUICInternalError
         ClientCleartextType              -> runGetOrFailWithError bs getClientClearText      QUICInternalError
         ZeroRTTProtectedType             -> runGetOrFailWithError bs getZeroRTTProtected     QUICInternalError
         OneRTTProtectedKeyPhaseZeroType  -> runGetOrFailWithError bs getOneRTTProtectedKeyPhaseZero  QUICInternalError
         OneRTTProctectedKeyPhaseOneType  -> runGetOrFailWithError bs getOneRTTProtectedKeyPhaseOne   QUICInternalError
         PublicResetType                  -> runGetOrFailWithError bs getPublicReset QUICInternalError

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
     decodeStreamFrame  ctx f ss oo d bs = runGetOrFailWithError bs
        (getStreamFrame ctx f ss oo d )
        QUICInvalidStreamData

     decodeAckFrame ctx n lack abl bs =  runGetOrFailWithError bs
        (getAckFrame n lack abl)
        QUICInvalidAckData

     decodeMaxDataFrame bs = runGetOrFailWithError bs
        getMaxDataFrame
        QUICInvalidAckData

     decodeMaxStreamDataFrame ctx bs= runGetOrFailWithError bs
        (getMaxStreamDataFrame ss)
        QUICInternalError
      where
          ss = decodeContextStreamSize ctx
     decodeMaxStreamIdFrame ctx bs= runGetOrFailWithError bs
        (getMaxStreamDataFrame ss)
        QUICInternalError
       where
          ss = decodeContextStreamSize ctx
     decodeBlockedFrame bs  = Right (Blocked, bs)
     decodePaddingFrame bs  = Right (Padding, bs)
     decodePingFrame    bs  = Right (Ping,    bs)
     decodeStreamIdNeededFrame ctx bs = Right (StreamIdNeeded, bs)
     decodeStreamBlockedFrame ctx bs = runGetOrFailWithError bs
        (getStreamBlockedFrame ss)
        QUICInternalError
      where
        ss = decodeContextStreamSize ctx
     decodeRstStreamFrame ctx bs = runGetOrFailWithError bs
        (getRstStreamFrame ss oo)
        QUICInvalidRstStreamData
      where
        ss = decodeContextStreamSize ctx
        oo = decodeContextOffsetSize ctx
     decodeNewConnectionIdFrame ctx bs = runGetOrFailWithError bs
        getNewConnectionId
        QUICInternalError
     decodeConnectionCloseFrame ctx bs= runGetOrFailWithError bs
        getConnectionCloseFrame
        QUICInvalidConnectionCloseData
     decodeGoawayFrame ctx bs =  runGetOrFailWithError bs
        (getGoawayFrame ss)
        QUICInvalidGoawayData
      where
        ss = decodeContextStreamSize ctx


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
encodeLongPacketPayload ctx p = runPutStrict $ putLongPackerPaload ctx p

-- | encodeHeader
encodeHeader :: EncodeContext -> Header -> ByteString
encodeHeader ctx hdr = runPutStrict $ putHeader ctx hdr

-- | encodeFrames
encodeFrames :: EncodeContext -> [Frame] -> ByteString
encodeFrames _ []       = BS.empty
encodeFrames ctx (f:fs) = (encodeFrame ctx f) `BS.append` encodeFrames ctx fs

-- | encodeFrame
encodeFrame :: EncodeContext -> Frame -> ByteString
encodeFrame ctx frame = runPutStrict $ putFrame ctx frame
