module Network.QUIC.Connection
  (
    DataPool
  , DataMap
  , StreamData
  , ChannelMap

  , emptyDataPool
  )
  where
import           Data.ByteString         (ByteString)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as M
import           Data.Maybe

import           Control.Concurrent.MVar
import           Network.QUIC.Types

-- | DataPool is that hold  maping connection-id to  data on a endpoint.
type DataPool = M.Map ConnectionId (MVar DataMap)

-- | DataMap is that hold data par stream  in each connection triple.
type DataMap = M.Map StreamId (MVar StreamData)

-- | Stream Data is that Map on a stream to accept out of order data.
type StreamData = M.Map Offset ByteString

-- | ChannelMap is that mapping connection id to bytestring in the
-- individual context.
type ChanelMap = M.Map ConnectionId (MVar ByteString)

--
-- DataPool Utils
--

emptyDataPool :: DataPool
emptyDataPool = M.empty


lookupDataPool :: DataPool -> ConnectionId -> IO (Maybe DataMap)
lookupDataPool pool c = undefined


---
--- DataMap Utils
---
initDataMap :: DataMap
initDataMap = M.empty

lookupDataMap :: DataMap -> StreamId -> IO (Maybe StreamData)
lookupDataMap map s = undefined

---
--- StreamData Utils
---

initStreamData :: StreamData
initStreamData = undefined
