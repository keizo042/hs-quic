module Main where

import           Network.QUIC

uname = "localhost"
uport = 8081

lhost = "localhost"
lport = 8082

cfg = undefined

main :: IO ()
main = do
    mgr <- newManager cfg
    handshake mgr
