module Network.QUIC.Codec.Put
  where

import qualified Data.Binary.Put             as Put
import           Data.Int
import           Data.Maybe

import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS

import           Network.QUIC.Codec.Internal
import qualified Network.QUIC.Internal       as I
import           Network.QUIC.Types

putPacketNumber :: PacketNumber -> Put.Put
putPacketNumber =  Put.putInt64be . fromIntegral

putConnectionId :: ConnectionId -> Put.Put
putConnectionId = Put.putWord64be . fromIntegral

putStreamId :: StreamId -> Put.Put
putStreamId sid
  | (sid < 2^8 ) = Put.putWord8 $ fromIntegral sid
  | (sid < 2^16) = Put.putInt16be $ fromIntegral sid
  | (sid < 2^24) = error "not yet implemented in Network.QUIC.Codec"
  | otherwise = Put.putInt32be $ fromIntegral sid

putOffset :: Offset -> Put.Put
putOffset offset
  | (offset < 2^16) = Put.putInt16be $ fromIntegral offset
  | (offset < 2^32) = Put.putInt32be $ fromIntegral offset
  | otherwise       = Put.putInt64be $ fromIntegral offset

-- | putErrorCode
putErrorCode :: ErrorCode -> Put.Put
putErrorCode e = Put.putInt32be . fromIntegral $ errorCodeToInt e

putQUICVersion :: QUICVersion ->  Put.Put
putQUICVersion v = Put.putInt32be $ fromIntegral v

putLongHeaderType :: LongHeaderType -> Put.Put
putLongHeaderType t = Put.putWord8  (fromLongHeaderType t)

putShortHeader c pn = Put.putWord8 0x00 >> putConnectionIdMaybe >> putPacketNumber pn
  where
    putConnectionIdMaybe = if isJust c then putConnectionId $ fromJust c else return ()

putLongHeader c pn v = Put.putWord8 0x80 >> putConnectionId c >> putPacketNumber pn >> putQUICVersion v

-- Long Packet Payload
-- | putVersionNegotiation

putVersionNegotiation v vs =  putQUICVersion v >> mapM_ putQUICVersion vs

putConnectionCloseFrame :: ErrorCode -> ByteString -> Put.Put
putConnectionCloseFrame e bs = putErrorCode e >> Put.putInt16be ( fromIntegral $ BS.length bs) >> Put.putByteString bs

putStreamFrame s o bs = putStreamId s >> putOffset o >> putStreamData bs
  where
    putStreamData bs = putStreamDataLength (BS.length bs) >> Put.putByteString bs
    putStreamDataLength 0 = return ()
    putStreamDataLength i = Put.putInt8 $ fromIntegral i

putAckFrame ctx lack delay blocks stamps=  undefined

putMaxDataFrame i = undefined

putGoawayFrame  latest unkown= undefined

putMaxStreamDataFrame sid size = undefined

putPaddingFrame = Put.putByteString $ BS.singleton 0x60

putMaxStreamIdFrame :: StreamId -> Put.Put
putMaxStreamIdFrame sid = putStreamId sid

