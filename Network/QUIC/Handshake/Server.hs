module Network.QUIC.Handshake.Server
  (
    handshakeServer
  )
  where

handshakeServer :: Context -> IO ()
handshakeServer ctx = do
    recvInitial ctx
    tlsHandshakeServer ctx

recvInitial :: Context -> IO ()
recvInitial = undefined

tlsHandshakeServer :: Context -> IO ()
tlsHandshakeServer = undefined
