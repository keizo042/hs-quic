module Network.QUIC.Time
  (
    encodeQUICTime
  , decodeQUICTime

  , subQUICTime
  , addQUICTime
  , diffQUICTime
  , fromUTCTime
  , toUTCTime

  )
  where


instance Real QUICTime where
    toRational = undefined


instance Integral QUICTime where
    quotRem = undefined
    toInteger = undefined

instance Num QUICTime where
    (+) = addQUICTime
    (-) = subQUICTime
    (*) = multiQUICTime
    abs = absQUICTime
    signum = signumQUICTime
    fromInteger = undefined


encodeQUICTime :: Int32 -> QUICTime
encodeQUICTime i = undefined

decodeQUICTime :: QUICTime -> Int
decodeQUICTime t = undefined

addQUICTime :: QUICTime -> QUICTime -> QUICTime
addQUICTime l r = encodeQUICTime $ l' + r'
  where
    l' = fromIntegral $ decodeQUICTime l
    r' = fromIntegral $ decodeQUICTime r

subQUICTime :: QUICTime -> QUICTime -> QUICTime
subQUICTime l r = encodeQUICTime $ l' - r'
  where
    l' = fromIntegral $ decodeQUICTime l
    r' = fromIntegral $ decodeQUICTime r

diffQUICTime :: QUICTime -> QUICTime -> AckTimeDelta
diffQUICTime l r  = undefined

multiQUICTime :: QUICTime -> QUICTime -> QUICTime
multiQUICTime l r = encodeQUICTime $ l' * r'
  where
    l' = fromIntegral $ decodeQUICTime l
    r' = fromIntegral $ decodeQUICTime r

absQUICTime :: QUICTime -> QUICTime
absQUICTime t = encodeQUICTime $ abs $ fromIntegral $ decodeQUICTime t

signumQUICTime = undefined

quicTimeToInteger :: QUICTime -> Int
quicTimeToInteger = undefined

addQUICTimeAckDelta :: QUICTime -> AckTimeDelta -> QUICTime
addQUICTimeAckDelta time delta = undefined

toUTCTime :: QUICTime -> UTCTime
toUTCTime = undefined

fromUTCTime :: UTCTime -> QUICTime
fromUTCTime = undefined
