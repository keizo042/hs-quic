module Network.QUIC.Keys
  (
    KeyState(..)
  )
  where

data KeyState = KeyState {
                keyStateTranscriptHash :: ByteString
              , keyStateRandom         :: Int
              }
              deriving (Show, Eq)




