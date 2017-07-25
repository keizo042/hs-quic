module Network.QUIC.Codec.Put
  where

import qualified Data.Binary.Put             as Put
import           Data.Int

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

putQUICVersion :: QUICVersion ->  Put.Put
putQUICVersion v = undefined

putLongHeaderType :: LongHeaderType -> Put.Put
putLongHeaderType t = Put.putWord8  (fromLongHeaderType t)

-- | putErrorCode
putErrorCode :: ErrorCode -> Put.Put
putErrorCode e = Put.putInt32be . fromIntegral $ errorCodeToInt e
