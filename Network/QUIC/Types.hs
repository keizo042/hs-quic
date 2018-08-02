module Network.QUIC.Types (
                          Frame(..)
                          ) where


data Payload

data ProtectedPayload

data Frame = PADDING
           | RST_STREAM RstStreamFrame
           | CONNECTION_CLOSE ConnectionCloseFrame
           | APPLICATION_CLOSE ApplicationCloseFrame
           | MAX_DATA MaxDataFrame
           | MAX_STREAM_DATA MaxStreamDataFrame
           | PING
           | BLOCKED BlockedFrame
           | STREAM_BLOCKED StreamBlockedFrame
           | STREAM_ID_BLOCKED StreamIdBlockedFrame
           | NEW_CONNECTION_ID NewConnectionIdFrame
           | STOP_SENDING StopSendingFrame
           | ACK AckFrame
           | STREAM StreamFrame
           deriving (Eq, Show)

data RstStreamFrame

data ConnectionCloseFrame

data ApplicationCloseFrame

data MaxStreamDataFrame

data BlockedFrame

data StreamBlockedFrame

data MaxDataFrame

data MaxStreamDataFrame

data BlockedFrame

data StreamBlockedFrame

data StreamIdBlockedFrame

data StopSendingFrame

data AckFrame

data StreamFrame
