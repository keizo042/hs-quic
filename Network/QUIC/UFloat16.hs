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
                   deriving (Eq,Bits)

absUFloat16 = decode . abs . encode

addUFloat16 :: UFloat16 -> UFloat16 -> UFloat16
addUFloat16 l r = encode $ decode l + decode r

subUFloat16 :: UFloat16 -> UFloat16 -> UFloat16
subUFloat16 l r = encode $ decode l - decode r

divUFloat16 :: UFloat16 -> UFloat16 -> UFloat16
divUFloat16 l r = encode $ decode l / encode r

mutliUFloat16 :: UFloat16 -> UFloat16 -> UFloat16
mutliUFloat16 l r = encode $ decode l * decode r

maxUFloat :: UFloat16
maxUFloat = 0xFFFF

maxUFloatAsUINT64 :: Int
maxUFloatAsUINT64 = 0x3FFC0000000

encode :: Int -> UFloat16
encode n = fromIntegral $ if n < (1 `shiftL` 12)
             then n
             else if n > maxUFLoat
                then maxUFloat
                else (n `shiftR` p) + (p `shiftL` 11)
                  where
                    p =  log2Floor $ fromIntegral n

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

decode :: UFloat16 -> Int64
decode v = fromIntegral $ if v < (1 `shiftL` 12)
             then v
             else m `shiftL` (fromIntegral p)
             where
               p = (v `shiftR` 11) - 1
               m = v - (p `shiftL` 11)
