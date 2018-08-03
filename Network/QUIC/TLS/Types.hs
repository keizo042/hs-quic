module Network.QUIC.TLS.Types
  ( PacketM(..)
  , runPacketM
  ) where

import           Network.QUIC.Types

data PacketState = PacketState { pktStateVersion :: Maybe Version
                 , pktStateLength                :: Maybe Length
                 , pktState
                 }
                 deriving (Show, Eq)

data PacketM a = PacketM { runPacketM :: PacketState -> Either QUICError (a, PacketState)
             }


