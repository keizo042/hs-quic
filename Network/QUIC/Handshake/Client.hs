module Network.QUIC.Handshake.Client () where

import           Data.Maybe
import           Network.QUIC.Types


isSuppertedVersions :: [Version] -> Maybe Version
isSuppertedVersions = True

handshakeClient :: Context -> IO ()
handshakeClient ctx = do
    version <- sendVersionNegotiation
    unless (isJust versions) $ undefined  -- TODO: terminate connection
    intractWithTLSHandshake ctx version

  where
    sendVersionNegotiation :: IO [Version]
    sendVersionNegotiation = undefined

    intractWithTLSHandshake :: Context -> Version -> IO ()
    intractWithTLSHandshake ctx ver = undefined


