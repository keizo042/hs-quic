{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.QUIC.Time
  where
import           Data.ByteString        (ByteString)
import           Data.Int
import qualified Data.Time.Clock        as Clock

import           Network.QUIC.Types.Ack

-- | QUICTime
-- TODO: See [draft-ietf-quic-transport-04] Section 8.2.2.1 "Time Format"
-- We Must implement IEEE74 like time format and the utility.
-- 16bit unsinged float.
-- with
-- mantissa 11 bit
-- exponent 5 bit
-- time in microseconds
newtype QUICTime =  QUICTime Int32
                   deriving (Show, Eq)

instance Num QUICTime where
    (+) = addQUICTime
    (-) = subQUICTime
    (*) = multiQUICTime
    abs = absQUICTime
    signum = signumQUICTime


parseTime :: ByteString -> QUICTime
parseTime bs = undefined
  where
    mantissa = undefined
    exponent = undefined

addQUICTime :: QUICTime -> QUICTime -> QUICTime
addQUICTime = undefined

subQUICTime :: QUICTime -> QUICTime -> QUICTime
subQUICTime = undefined

multiQUICTime :: QUICTime -> QUICTime -> QUICTime
multiQUICTime = undefined

absQUICTime = undefined
signumQUICTime = undefined
toInt = undefined

diffQUICTime :: QUICTime -> QUICTime -> AckTimeDelta
diffQUICTime lv rv = undefined

toUTCTime :: QUICTime -> Clock.UTCTime
toUTCTime = undefined

fromUTCTime :: Clock.UTCTime -> QUICTime
fromUTCTime = undefined
