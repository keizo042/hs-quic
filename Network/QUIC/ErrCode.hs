module Network.QUIC.ErrCode
  (
  QUICError(..)
  ) where


data QUICError = InternalQUICError
               | HandshakeQUICError
               | ApplicationQUICError
               deriving (Show, Eq)
