module Network.QUIC.Handshake.Client
  (
    handshakeClient
  ) where

import           Data.Maybe
import           Network.QUIC.Types
import qualified Network.TLS        as TLS


handshakeClient :: Context -> IO ()
handshakeClient ctx = do
    sendInitial ctx
    tlsHandshakeClient ctx

-- TODO: select supported version
isSuppertedVersions :: [Version] -> Maybe Version
isSuppertedVersions = True

sendInitial :: Context -> IO ()
sendInitial ctx = do
    p <- recvPacket ctx
    case p of
      (PacketRetry pktRetry)            -> undefined -- TODO: Retry Initial Packet with Retry token and destination connection id.
      (PacketHandshake pktHandshake)    -> undefined -- TODO: modify state and start TLS Handshake Protocol.
      (PacketVersionNegotiation pktVer) -> undefined -- TODO: start version negotiation.
      _                                 -> undefined -- TODO: Throw Error as handshake error


tlsHandshakeClient :: Context -> IO ()
tlsHandshakeClient ctx = undefined
-- TODO: after recv Handshake Packet with Long header.
-- start Handshake Protocol on CRYPTO Frame with AEAD.
