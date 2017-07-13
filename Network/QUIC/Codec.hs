module Network.QUIC.Codec
  (
  encode,
  decode
  )
  where

import           Network.QUIC
import           Network.QUIC.Types

toHeaderType :: Word8 -> HeaderType
toHeaderType b
  | (b == 0x80) = LongHeaderType
  | otherwise =  ShortHeaderType

fromHeaderType :: HeaderType -> Word8
fromHeaderType LongHeaderType  = 0x80
fromHeaderType ShortHeaderType = 0x00

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

fromLongHeaderType :: LongHeaderType -> Word8
fromLongHeaderType VersionNegotiationType          = 1
fromLongHeaderType ClientInitialType               = 2
fromLongHeaderType ServerStatelessRetryType        = 3
fromLongHeaderType ServerCleartextType             = 4
fromLongHeaderType ClientCleartextType             = 5
fromLongHeaderType ZeroRTTProtectedType            = 6
fromLongHeaderType OneRTTProtectedKeyPhaseZeroType = 7
fromLongHeaderType OneRTTProctectedKeyPhaseOneType = 8
fromLongHeaderType PublicResetType                 = 9

hasConnectionId :: Word8 -> Bool
hasConnectionId w = w .&. 0x40 ==  0x40

hasKeyPhase :: Word8 -> Bool
hasKeyPhase w = w .&. 0x20 == 0x20

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
           hasFin = undefined

           chkStreamId :: Word8 -> StreamSize
           chkStreamId = undefined

           chkOffset :: Word8 -> OffsetSize
           chkOffset = undefined

           hasData :: Word8 -> Bool
           hasData = undefined
      -- 0xc0 - 0xff
      -63  -> Just (AckType (hasNumBlocksField w) (chkLACKField w) (chkAckBlockLengthField w))
        where
          hasNumBlocksField :: Word8 -> Bool
          hasNumBlocksField = undefined

          chkLACKField :: Word8 -> LAckSize
          chkLACKField = undefined

          chkAckBlockLengthField :: Word8 -> AckBlockLengthSize
          chkAckBlockLengthField = undefined

fromFrameType :: FrameType -> Word8
fromFrameType = undefined

putStreamId :: StreamId -> Put.Put
putStreamId = undefined

getStreamId :: Int -> Get.Get StreamId
getStreamId 1 = I.getInt8
getStreamId 2 = I.getInt16
getStreamId 3 = I.getInt24
getStreamId 4 = I.getInt32

getOffset :: Int -> Get.Get Offset
getOffset 0 = return 0
getOffset 2 = fromIntegral <$> I.getInt16
getOffset 4 = fromIntegral <$> I.getInt32
getOffset 8 = fromIntegral <$> I.getInt64

putOffset :: Offset -> Put.Put
putOffset offset = undefined

decodeHeader :: ByteString -> QUICResult (Header, ByteString)
decodeHeader bs = undefined
  where
    decodeLongHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeLongHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                         Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                         Left _               -> undefined
      where
        decode' :: Get.Get Header
        decode' = undefined
    decodeShortHeader :: ByteString -> QUICResult (Header, ByteString)
    decodeShortHeader bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                          Right (rest, _, hdr) -> Right (hdr, LBS.toStrict rest)
                          Left _               -> undefined
      where
        decode' :: Get.Get Header
        decode' = undefined

encodeHeader :: Header -> ByteString
encodeHeader hdr = undefined

decodeFrames :: Header -> ByteString -> QUICResult [Frame]
decodeFrames hdr bs = case (decodeFrame hdr bs) of
                        Right (f,bs') -> case (decodeFrames hdr bs') of
                                     Right fs -> Right (f : fs)
                                     Left e   -> Left e
                        Left e  -> Left e


decodeFrame :: Header -> ByteString -> QUICResult (Frame, ByteString)
decodeFrame hdr bs = case (bitToFrameType $ BS.head bs) of
                       Nothing    -> undefined
                       Just ft  -> case ft of
                         st@(StreamType fin hasSID hasOffset d) ->
                            decodeStreamFrame fin hasSID hasOffset d st bs
                         at@(AckType n lack abl)                ->
                            decodeAckFrame hdr at bs
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
                          Bool -> -- has stream id field
                          Bool -> -- has offset field
                          Bool -> -- has body filed
                          ByteString -> -- payload body
                          QUICResult (Frame, ByteString)  -- a frame and rest payload or error code
     decodeStreamFrame  hdr fin hasSID hasOffset hasData bs = case (Get.runGetOrFail decode'  $ LBS.fromStrict bs) of
                                       (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                       _ -> undefined

        where
           decode' :: Get.Get Frame
           decode' = undefined -- Just <$> Stream <*> (getStreamId sn) <*> (getOffset ofn) <*> I.getInt16
     decodeAckFrame hdr _ bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeMaxDataFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeMaxStreamDataFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeMaxStreamIdFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeBlockedFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeStreamBlockedFrame  hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeStreamIdNeededFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodeRstStreamFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined

     decodePaddingFrame _ bs = Right (Padding, BS.tail bs)
     decodePingFrame _ bs = Right (Ping, BS.tail bs)
     decodeNewConnectionIdFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeConnectionCloseFrame hdr bs = case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined
     decodeGoawayFrame hdr bs= case (Get.runGetOrFail decode' $ LBS.fromStrict bs) of
                                   (Right (rest, _, f)) -> Right (f, LBS.toStrict rest)
                                   _ -> undefined
        where
          decode' :: Get.Get Frame
          decode' = undefined



encodeFrames :: [Frame] -> ByteString
encodeFrames []     = BS.empty
encodeFrames (f:fs) = encodeFrame f `BS.append` encodeFrames fs

encodeFrame :: Frame -> ByteString
encodeFrame f = LBS.toStrict  undefined
  where
    encodeStramFrame = undefined
    encodeAckFrame = undefined
    encodeAck = undefined

decode :: ByteString -> QUICResult Packet
decode bs = decode' $ decodeHeader bs
    where
    decode' (Left e) = Left e
    decode' (Right (hdr,bs')) = case (decodeFrames hdr bs') of
              (Right fs) -> Right (Packet hdr fs)
              (Left e)   -> Left e


encode :: Packet -> ByteString
encode (Packet hdr fs) = encodeHeader hdr `BS.append` encodeFrames fs

getPacketNumber :: Get.Get PacketNumber
getPacketNumber = undefined

putPacketNumber :: PacketNumber -> Put.Put
putPacketNumber = undefined

putConnectionId :: ConnectionId -> Put.Put
putConnectionId = Put.putWord64be . fromIntegral

getConnectionId :: Get.Get ConnectionId
getConnectionId = fromIntegral <$> I.getInt64
