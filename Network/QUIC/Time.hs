module Network.QUIC.Time
  where
import           Data.ByteString    (ByteString)
import qualified Data.Time.Clock    as Clock
import           Network.QUIC.Types
-- QUIC time format

parseTime :: ByteString -> QUICTime
parseTime bs = undefined
  where
    mantissa = undefined
    exponent = undefined

toUTCTime :: QUICTime -> Clock.UTCTime
toUTCTime = undefined

fromUTCTime :: Clock.UTCTime -> QUICTime
fromUTCTime = undefined
