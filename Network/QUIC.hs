module Network.QUIC
  (
  )
  where
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int
import           Data.Maybe            (Maybe)
import qualified Data.Maybe            as Maybe
import qualified Data.Time.Clock       as Clock
import           Data.Word             (Word8)
import qualified Network.QUIC.Internal as I
import           Network.QUIC.TLS
import           Network.QUIC.Types
import qualified Network.Socket        as S
import qualified Network.TLS           as TLS


limitPMTU :: Int -> Bool
limitPMTU i = upper i && lower i
      where
  upper i = i < 65527
  lower i = i > 1252

-- the number that library support protocol version.
supportedVersions :: [QUICVersion]
supportedVersions = undefined
--    [0x0d, 0x0e ]

-- latest number that the libary support
defaultVersion :: Int32
defaultVersion = 0x0e

-- QUIC Version  Negtiaton

isSupportedversion :: QUICVersion -> Bool
isSupportedversion e = e `elem` supportedVersions

-- testing
testDataMap = undefined
testStreamData = undefined
