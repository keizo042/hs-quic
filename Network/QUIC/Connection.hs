module Network.QUIC.Connection
  (
  )
  where
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict      as M
import           Data.Maybe
import           Network.QUIC.Types

insertStreamData = undefined

-- Stream Data Map , Key: Stream Id / Value: (Packet Number, Data)
-- the data is specific Stream Id Data

type StreamData = M.Map Offset ByteString

type DataMap = M.Map StreamId StreamData

initDataMap :: DataMap
initDataMap = M.empty

insertDataMap :: DataMap
              -> StreamId
              -> Offset
              -> ByteString
              -> DataMap
insertDataMap m sid ofs bs = modify' sid ofs bs sd m
    where
      sd = lookup' sid m
      lookup' ::  StreamId -> DataMap -> (Maybe StreamData)
      lookup' sid' m' = M.lookup sid' m'

      modify' :: StreamId -> Offset -> ByteString -> (Maybe StreamData) -> DataMap -> DataMap
      modify' sid' ofs' bs' sd0 m' = M.insert sid' sd1 m'
        where
          sd1 = case sd0 of
                    (Just sd) ->  M.insert ofs' bs' sd
                    Nothing   ->  M.insert ofs' bs' M.empty

dropDataMap :: StreamId -> DataMap -> DataMap
dropDataMap sid m = M.delete sid m

takeDataMap :: StreamId
            -> DataMap
            -> (DataMap, Maybe ByteString)
takeDataMap sid m = case (M.lookup sid m) of
                      (Just sd) -> (M.delete sid m, Just $ toBS sd)
                      (Nothing) -> (m, Nothing)
                where
                  toBS :: StreamData -> ByteString
                  toBS sd = foldl BS.append BS.empty $ map snd $ M.toList sd
                  f :: ByteString -> (PacketNumber, ByteString) -> ByteString
                  f b (k,v) = b `BS.append` v

