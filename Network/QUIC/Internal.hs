module Network.QUIC.Internal
  (
    getIntN
  , getInt8
  , getInt16
  , getInt24
  , getInt32
  , getInt48
  , getInt56
  , getInt64

  , putInt24
  ) where
import           Data.Binary
import qualified Data.Binary.Get as Get
import           Data.Bits
import           Data.Int        (Int8)

getIntN :: Int -> Get Int
getIntN 8  = getInt8
getIntN 16 = getInt16
getIntN 24 = getInt24
getIntN 32 = getInt32
getIntN 48 = getInt48
getIntN 56 = getInt56
getIntN 64 = getInt64
getIntN _  = error "unsupport number"

getInt8 :: Get Int
getInt8 = getIntNbyte 1

getInt16 :: Get Int
getInt16 = getIntNbyte 2

getInt24 :: Get Int
getInt24 = getIntNbyte 3

getInt32  :: Get Int
getInt32 = getIntNbyte 4

getInt48 :: Get Int
getInt48 = getIntNbyte 6

getInt56 :: Get Int
getInt56 = getIntNbyte 7

getInt64 :: Get Int
getInt64 = getIntNbyte 8


getIntNbyte :: Int -> Get Int
getIntNbyte 0 = return 0
getIntNbyte n = foldl f 0 <$> list
  where
    f :: Int -> (Int8, Int) -> Int
    f n (x,i) = n + (shiftL (i * 8) $ toInt x)

    list :: Get [(Int8, Int)]
    list = (\xs -> zip xs [0..])  <$> (sequence $ replicate n Get.getInt8)

    toInt = fromIntegral . toInteger

putInt24 :: Int -> Put
putInt24 n = error "put utlitiy"
