module Main where

import           Control.Monad.Exception
import           Network.QUIC
import           Network.Socket

body = "hello world"

main :: IO ()
main = do
    let name  = "localhost"
        port = 8081
    sock <- newUDPSocket name port
    ctx <- contextNew sock defaultQUICParams
    handshake ctx
    bs <- sendData ctx body
    putStrLn bs


newUDPSocket :: String -> PortNumber -> IO Socket
newUDPSocket host port = undefined -- TODO: create upd socket. ref: https://gist.github.com/keizo042/b09bbfd263b61fa180256d14c41810e6
