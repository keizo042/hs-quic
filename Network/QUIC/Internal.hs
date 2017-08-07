module Network.QUIC.Internal
  (
    contextClose
  , contextNew
  , contextSend
  , contextRecv
  ) where

import           Data.ByteString
import           Network.QUIC.Types

contextClose :: Context -> IO ()
contextClose ctx = undefined

-- TODO: type annoate
contextNew ctx = undefined

contextSend :: Context -> ByteString -> IO ()
contextSend ctx bs = undefined

contextRecv :: Context -> Int -> IO ByteString
contextRecv ctx n = undefined
