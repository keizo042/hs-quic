module Network.QUIC.Internal
  (
  )
  where

import           Data.Int
import           Network.QUIC.Types


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
