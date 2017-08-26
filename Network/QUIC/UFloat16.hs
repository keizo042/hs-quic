{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.QUIC.UFloat16
  (
    UFloat16
  , encode
  , decode
  , maxUFloat
  , maxUFloatAsUINT64
  )
  where

import qualified Data.Array as A
import           Data.Bits
import           Data.Int

newtype UFloat16 = UFloat16 Int16
                   deriving (Show, Eq, Bits, Enum, Ord, Integral,Num, Real)

absUFloat16 uf = encode $ abs $ decode uf

addUFloat16 :: UFloat16 -> UFloat16 -> UFloat16
addUFloat16 l r = encode $ l' + r'
  where
    l' = decode l
    r' = decode r

subUFloat16 :: UFloat16 -> UFloat16 -> UFloat16
subUFloat16 l r = encode $ l' - r'
  where
    l' = decode l
    r' = decode r


maxUFloat :: UFloat16
maxUFloat = 0xFFFF

maxUFloatAsUINT64 :: Int
maxUFloatAsUINT64 = 0x3FFC0000000

-- |
-- encode is that from 64 bit signed int to unsigned float
--
-- >>> (decode . encode 0x42) == 0x42
-- True
encode :: Int -> UFloat16
encode n = if n < (1 `shiftL` 12) then fromIntegral n else v
          where
            v :: UFloat16
            v   = if (n > (fromIntegral maxUFloat)) then maxUFloat else v'
            v' :: UFloat16
            v'  = fromIntegral $ (n `shiftR` p) + (p `shiftL` 11)
            p   =  log2Floor $ fromIntegral n

log2Floor :: Int -> Int
log2Floor i = debrujin A.! (x `shiftR` 27)
  where
    x = i .|. i `shiftR` 1
          .|. i `shiftR` 2
          .|. i `shiftR` 4
          .|. i `shiftR` 8
          .|. i `shiftR` 16
    debrujin = A.listArray (1, length tbl - 1) tbl
    tbl = [ 0, 9, 1, 10, 13, 21, 2, 29,
            11, 14, 16, 18, 22, 25, 3, 30,
            8, 12, 20, 28, 15, 17, 24, 7,
            19, 27, 23, 6, 26, 5, 4, 31 ]

decode :: UFloat16 -> Int
decode v = if v < (1 `shiftL` 12) then fromIntegral v else i
             where
               v' :: Int
               v' = fromIntegral v
               i :: Int
               i = m `shiftL` p
               m = v' - (p `shiftL` 11)
               p :: Int
               p = (v' `shiftR` 11) - 1
