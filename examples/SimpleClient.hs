module Main where

import qualified Data.ByteString as BS
import           Network.QUIC

uname = "localhost"
uport = 8081

lhost = "localhost"
lport = 8082

msg = "hello world"

cfg = undefined

main :: IO ()
main = do
    mgr <- newManager cfg
    handshake mgr
    con <- send mgr uhost uport msg
    m <- recv mgr con
    BS.putStrLn m
    closeManager mgr
