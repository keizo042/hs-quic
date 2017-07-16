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
              Right (hdr, bs') -> decode' hdr bs'
    where
      decode' hdr bs = case (decodeFrames hdr bs) of
                (Right fs) -> Right (Packet hdr fs)
                (Left e)   -> Left e


-- | encode is a API to encode to Packet of QUIC.
encode :: Packet -> ByteString
encode (Packet hdr fs) = encodeHeader hdr `BS.append` encodeFrames fs

decodeFrames :: Header -> ByteString -> QUICResult [Frame]
decodeFrames hdr bs = case (decodeFrame hdr bs) of
                        Right (f,bs') -> case (decodeFrames hdr bs') of
                                     Right fs -> Right (f : fs)
                                     Left e   -> Left e
                        Left e  -> Left e

encodeFrames :: [Frame] -> ByteString
encodeFrames []     = BS.empty
encodeFrames (f:fs) = encodeFrame f `BS.append` encodeFrames fs

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
        decode' = undefined
    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                          Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                          Left _               -> Left QUICInvalidPacketHeader
      where
        decode' :: Get.Get Header
        decode' = undefined



decodeFrame :: Header -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame hdr bs = case (toFrameType $ BS.head bs) of
                       Nothing    -> undefined
                       Just ft  -> case ft of
                         st@(StreamType f ss oo d)  -> decodeStreamFrame hdr f ss oo d bs
                         at@(AckType n lack abl)    -> decodeAckFrame hdr at bs
                         MaxDataType          -> decodeMaxDataFrame hdr bs
                         MaxStreamDataType    -> decodeMaxStreamDataFrame hdr bs
                         MaxStreamIdType      -> decodeMaxStreamIdFrame hdr bs
                         StreamBlockedType    -> decodeStreamBlockedFrame hdr bs
                         StreamIdNeededType   -> decodeStreamIdNeededFrame hdr bs
                         RstStreamType        -> decodeRstStreamFrame hdr bs
                         PaddingType          -> decodePaddingFrame hdr bs
                         PingType             -> decodePingFrame hdr bs
                         NewConnectionType    -> decodeNewConnectionIdFrame hdr bs
                         ConnectionCloseType  -> decodeConnectionCloseFrame hdr bs
   where
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
           decode' = undefined -- Just <$> Stream <*> (getStreamId sn) <*> (getOffset ofn) <*> I.getInt16
     decodeAckFrame hdr _ bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidAckData
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeMaxDataFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidAckData
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeMaxStreamDataFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeMaxStreamIdFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeBlockedFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidBlockedData
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeStreamBlockedFrame  hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeStreamIdNeededFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidStreamId
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeRstStreamFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidRstStreamData
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodePaddingFrame _ bs = Right (Padding, BS.tail bs)
     decodePingFrame _ bs = Right (Ping, BS.tail bs)
     decodeNewConnectionIdFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInternalError
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeConnectionCloseFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidConnectionCloseData
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeGoawayFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> Left QUICInvalidGoawayData
        where
          decode' :: Get.Get Frame
          decode' = undefined



encodeHeader :: Header -> ByteString
encodeHeader (LongHeader c pktn v) = LBS.toStrict $ Put.runPut $ do
                                  putConnectionId c
                                  putPacketNumber c
                                  putQUICVersion v

encodeHeader (ShortHeader c pktn)   = LBS.toStrict $ Put.runPut $ do
                                  if Maybe.isJust c
                                    then putConnectionId $ Maybe.fromJust c
                                    else return ()
                                  putPacketNumber pktn

encodeFrame :: Frame -> ByteString
encodeFrame f = LBS.toStrict  undefined
  where
    encodeStramFrame = undefined
    encodeAckFrame = undefined
    encodePaddingFrame = undefined
    encodeGoawayFrame = undefined


getPacketNumber :: Get.Get PacketNumber
getPacketNumber = undefined

putPacketNumber :: PacketNumber -> Put.Put
putPacketNumber = undefined

getConnectionId :: Get.Get ConnectionId
getConnectionId = fromIntegral <$> I.getInt64

putConnectionId :: ConnectionId -> Put.Put
putConnectionId = Put.putWord64be . fromIntegral

getStreamId :: Int -> Get.Get StreamId
getStreamId 1 = I.getInt8
getStreamId 2 = I.getInt16
getStreamId 3 = I.getInt24
getStreamId 4 = I.getInt32

putStreamId :: StreamId -> Put.Put
putStreamId sid
  | (sid < 2^8 ) = Put.putWord8 $ fromIntegral sid
  | (sid < 2^16) = Put.putInt16be $ fromIntegral sid
  | (sid < 2^24) = error "not yet implemented in Network.QUIC.Codec"
  | otherwise = Put.putInt32be $ fromIntegral sid

getOffset :: Int -> Get.Get Offset
getOffset 0 = return 0
getOffset 2 = fromIntegral <$> I.getInt16
getOffset 4 = fromIntegral <$> I.getInt32
getOffset 8 = fromIntegral <$> I.getInt64

putOffset :: Offset -> Put.Put
putOffset offset
  | (offset < 2^16) = Put.putInt16be $ fromIntegral offset
  | (offset < 2^32) = Put.putInt32be $ fromIntegral offset
  | otherwise       = Put.putInt64be $ fromIntegral offset

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
