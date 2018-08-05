module Main where

import           Control.Monad.Exception
import           Network.QUIC
import           Network.Socket

main :: IO ()
main = do
    let hostname = "localhost"
        port = "8081"
    sock <- newUDPSocket
    ctx <- contextNew sock serverParams
    runEcho ctx `catch` (\ SomeException e  -> print e >> contextClose ctx)


runEcho :: Context -> IO ()
runEcho ctx = do
    bs <- recvData ctx
    sendData ctx $ "echo: " ++ bs
    runEcho ctx

newUDPSocket :: String -> PortNumber -> IO Socket
newUDPSocket host port = undefined -- TODO: create upd socket. ref: https://gist.github.com/keizo042/b09bbfd263b61fa180256d14c41810e6
