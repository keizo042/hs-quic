module Network.QUIC.Keys
  (
    KeyState(..)
  )
  where

data KeyState = KeyState { keyStateTranscriptHash :: ByteString
              }
              deriving (Show, Eq)




