module Network.QUIC.Types.Ack
  where

import           Network.QUIC.UFloat16
-- | AckBlock is Blocks that is recived  in Ack Frame.
data AckBlock = AckBlock [PacketNumber]
              deriving Show

-- | Gap is that gap between previous lost packet and latest one in Ack
-- Frame.
type Gap = Int

-- | AckTimeStamp is TimeStamp represent in Ack Frame.
data AckTimeStamp = AckTimeStamp [(PacketNumber, QUICTime)]
                  deriving Show

type AckTimeDelta = UFloat16
