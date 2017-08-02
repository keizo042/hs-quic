module Network.QUIC.Codec.Internal
  where

import qualified Network.QUIC.Internal as I
import           Network.QUIC.Types

import           Data.Bits
import           Data.Word

--
-- the Internal package is codec bit checkers.
-- and convert from/to type/word8.
--
--

toDecodeContext :: Header -> DecodeContext
toDecodeContext h = undefined

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
      -- draft indicate 0xc0 - 0xff is Ack Frame.
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
                                       0x03 -> AckBlock6Byte

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
fromFrameType (StreamType f ss oo d) = 0xa0 - 0xbf
                                    .|. fin f
                                    .|. stream ss
                                    .|. offset oo
                                    .|. adata d
  where
    fin True  = 0x20
    fin False = 0x00

    stream Stream1Byte =0x00
    stream Stream2Byte =0x08
    stream Stream3Byte =0x10
    stream Stream4Byte =0x18

    offset NoExistOffset = 0x00
    offset Offset2Byte   = 0x02
    offset Offset4Byte   = 0x04
    offset Offset8Byte   = 0x06

    adata True  = 0x01
    adata False = 0x00

fromFrameType (AckType n ll mm) =  0xc0 - 0xff
                              .|. nblock n
                              .|. len ll
                              .|. ablk mm
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
    ablk AckBlock6Byte = 0x03

-- | hasConnectionId check existing ConnectionId Flag in Header.
hasConnectionId :: Word8 -> Bool
hasConnectionId w = w .&. 0x40 ==  0x40

-- | hasKeyPhase check existing Key Phase Flag in Header.
hasKeyPhase :: Word8 -> Bool
hasKeyPhase w = w .&. 0x20 == 0x20

toPacketNumberSize :: Word8 -> PacketNumberSize
toPacketNumberSize w
  | w .&. 0x01 == 0x01 = PacketNumber1Byte
  | w .&. 0x02 == 0x02 = PacketNumber2Byte
  | w .&. 0x03 == 0x03 = PacketNumber4Byte
  | otherwise =  PacketNumber4Byte


toAckBlockLengthSize :: Int -> AckBlockLengthSize
toAckBlockLengthSize i = undefined
toLAckSize :: PacketNumber -> LAckSize
toLAckSize i = undefined


errorCodeToInt :: ErrorCode -> Int
errorCodeToInt (ApplicationErrorCode i) = i
errorCodeToInt (HostLocalErrorCode i)   = i
errorCodeToInt (QUICErrorCode err)      = quicErrorToInt err
errorCodeToInt (CrypotograhicError i)   = i

intToErrorCode :: Int -> ErrorCode
intToErrorCode i
 | (0x00000000 <= i && i <= 0x3FFFFFFF) = ApplicationErrorCode i
 | (0x40000000 <= i && i <= 0x7FFFFFFF) = HostLocalErrorCode i
 | (0x80000000 <= i && i <= 0xBFFFFFFF) = QUICErrorCode (intToQUICError i)
 | (0xC0000000 <= i && i <= 0xFFFFFFFF) = CrypotograhicError i

quicErrorToInt :: QUICError -> Int
quicErrorToInt QUICInternalError              = 0x80000001
quicErrorToInt QUICStreamDataAfterTermination = 0x80000002
quicErrorToInt QUICInvalidStreamData          =  undefined
quicErrorToInt _                              =  undefined

intToQUICError :: Int -> QUICError
intToQUICError i = f (i - 0x80000000)
  where
    f :: Int -> QUICError
    f 0x01 = QUICInternalError
    f 0x02 = QUICStreamDataAfterTermination
    f 0x03 = QUICInvalidPacketHeader
    f 0x04 = QUICInvalidFrameData
    f 0x05 = QUICMultipleTerminationOffsets
    f 0x06 = QUICStreamCancelled
    f 0x07 = QUICClosedCriticalStream
    f 0x30 = QUICMissingPayload
    f 0x2e = QUICInvalidStreamData
    f 0x3d = QUICUnencryptedStreamData
    f 0x59 = QUICMaybeCorruptedMemory
    f 0x06 = QUICInvalidRstStreamData
    f 0x07 = QUICInvalidConnectionCloseData
    f 0x08 = QUICInvalidGoawayData
    f 0x39 = QUICInvalidWindowUpdateData
    f 0x3a = QUICInvalidBlockedData
    f 0x4e = QUICInvalidPathCloseData
    f 0x09 = QUICInvalidAckData
    f 0x0a = QUICInvalidVersionNegotiationPacket
    f 0x0b = QUICInvalidPublicReset
    f 0x0c = QUICDecryptionFailure
    f 0x0d = QUICEncryptionFailure
    f 0x0e = QUICPacketTooLarge
    f 0x10 = QUICPeerGoingAway
    f 0x11 = QUICInvalidStreamId
    f 0x31 = QUICInvalidPriority
    f 0x12 = QUICTooManyOpenStreams
    f 0x4c = QUICTooManyAvailableStreams
    f 0x13 = QUICPublicReset
    f 0x14 = QUICInvalidVersion
    f 0x16 = QUICInvalidHeaderId
    f 0x17 = QUICInvalidNegotiationValue
    f 0x18 = QUICDecompressionFailure
    f 0x19 = QUICNetworkIdleTimeout
    f 0x43 = QUICHandshakeTimeout
    f 0x2a = QUICErrorMigratingAddress
    f 0x56 = QUICErrorMigratingPort
    f 0x32 = QUICEmptyStreamFrameNoFin
    f 0x3b = QUICFlowControlRecievedTooMuchData
    f 0x3f = QUICFlowControlSentTooMuchData
    f 0x40 = QUICFlowControlInvalidWindow
    f 0x3e = QUICConnectionIpPooled
    f 0x44 = QUICTooManyOutstandingSentPackets
    f 0x45 = QUICTooManyOutstandingRecievedPackets
    f 0x46 = QUICConnectionCancelled
    f 0x47 = QUICBadPacketLossRate
    f 0x49 = QUICPublicResetPostHandshake
    f 0x4a = QUICTimeoutsWithOpenStreams
    f 0x55 = QUICTooManyRTOs
    f 0x2c = QUICEncryptionLevelIncorrect
    f 0x37 = QUICVersionNegotiationMissmatch
    f 0x50 = QUICIpAddressChanged
    f 0x51 = QUICAddressValidationFailure
    f 0x5d = QUICTooManyFrameGaps
    f 0x60 = QUICTooManySessionsOnServers
    f _    = error "there are some conflict erro code"
