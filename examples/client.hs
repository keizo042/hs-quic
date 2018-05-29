module Main where

import qualified Data.ByteString as BS
import qualified Network.QUIC.Client as C

name  = "localhost"
port = 8081
body = "hello world"

main :: IO ()
main = do
    conn <- C.connect name port
    msg <- C.send body 
    BS.putStrLn msg
    C.close conn
