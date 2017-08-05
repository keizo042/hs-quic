module Network.QUIC.TLS
  (
  )
  where

import           Data.ByteString        (ByteString)
import           Data.Int
import           Data.Word

import           Network.TLS

import           Network.QUIC.TLS.Types
import           Network.QUIC.Types

-- entity of transport parameters.
-- TODO: the config will be replaced hash map structure.
-- key: the digit , value: numerical value 0x000 ~ 0xfeff
data Config = Config { configMaxStreamData :: Int32
            , configMaxData                :: Int32
            , configMaxStreamId            :: StreamId
            , configTimeout                :: Int16
            , configTruncateConnectionId   :: ConnectionId
            , configMaxPacketSize          :: Int16}
            deriving Show
