module Network.QUIC.Types where

-- | Token is a connection token.
-- | it is used Initial/Retry Phase.
data Token = Token
    deriving (Show, Eq)

data Length = Length1 | Length2 | Length4 | Length8
            deriving (Show, Eq)

data Version = Version
             deriving (Show, Eq)

type PacketNumber = Integer

type ConnectionID = Integer

data Offset = Offset
            deriving (Show, Eq)

data StreamID = StreamID
              deriving (Show, Eq)


data PlainPayload = PlainPayload
                  deriving(Show, Eq)

data ProtectedPayload = ProtectedPayload
                      deriving (Show, Eq)

data Frame = PADDING
           | RST_STREAM RstStreamFrame
           | CONNECTION_CLOSE ConnectionCloseFrame
           | APPLICATION_CLOSE ApplicationCloseFrame
           | MAX_DATA MaxDataFrame
           | MAX_STREAM_DATA MaxStreamDataFrame
           | MAX_STREAM_ID MaxStreamIdFrame
           | PING
           | BLOCKED BlockedFrame
           | STREAM_BLOCKED StreamBlockedFrame
           | STREAM_ID_BLOCKED StreamIdBlockedFrame
           | NEW_CONNECTION_ID NewConnectionIDFrame
           | STOP_SENDING StopSendingFrame
           | ACK AckFrame
           | ACK_ECN
           | PATH_CHALEENGE
           | PATH_RESPONSE
           | NEW_TOKEN
           | STREAM StreamFrame
           | CRYPTO CryptoFrame
           deriving (Eq, Show)

data RstStreamFrame = RstStreamFrame
    deriving (Show,Eq)

data ConnectionCloseFrame = ConnectionCloseFrame
    deriving (Show,Eq)

data ApplicationCloseFrame = ApplicationCloseFrame
    deriving (Show,Eq)

data MaxDataFrame = MaxDataFrame
    deriving (Show,Eq)

data MaxStreamDataFrame = MaxStreamDataFrame
    deriving (Show,Eq)

data MaxStreamIdFrame = MaxStreamIdFrame
    deriving (Show,Eq)

data BlockedFrame = BlockedFrame
    deriving (Show,Eq)

data StreamBlockedFrame = StreamBlockedFrame
    deriving (Show,Eq)

data StreamIdBlockedFrame = StreamIdBlockedFrame
    deriving (Show,Eq)

data NewConnectionIDFrame = NewConnectionIDFrame
    deriving (Show,Eq)

data StopSendingFrame = StopSendingFrame
    deriving (Show,Eq)

data AckFrame = AckFrame
    deriving (Show,Eq)

data AckEcnFrame = AckEcnFrame
    deriving (Show,Eq)

data StreamFrame = StreamFrame
    deriving (Show,Eq)

data CryptoFrame = CryptoFrame
    deriving (Show,Eq)

data ConnectionState = ConnectionState {
                       connStateConnectionID :: Maybe ConnectionID
                     , connStateLAcked       :: PacketNumber
                     }

data StreamState = StreamState {
                streamStateStreamID :: StreamID
                }

data KeyState = KeyState
