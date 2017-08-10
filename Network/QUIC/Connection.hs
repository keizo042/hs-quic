module Network.QUIC.Connection
  (
  )
  where
import           Data.ByteString         (ByteString)

import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Map.Strict         as M
import           Data.Maybe

import           Control.Concurrent.MVar
import           Network.QUIC.Types
