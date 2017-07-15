module Network.QUIC
  (
    ManagerSetting(..),
    Manager(..),
    newManager,
    closeManager,
    withManager,

    Packet(..),
    Header(..),
    Frame(..),

    TransportParameters(..),
    TransportParameter(..),

-- Testing
    testDataMap,
    testStreamData
  )
  where
import           Control.Monad
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as LBS
import           Data.Int
import qualified Data.Map.Strict       as M
import           Data.Maybe            (Maybe)
import qualified Data.Maybe            as Maybe
import qualified Data.Time.Clock       as Clock
import           Data.Word             (Word8)
import qualified Network.QUIC.Internal as I
import           Network.QUIC.Types
import qualified Network.Socket        as S
import qualified Network.TLS           as TLS

data ManagerSetting = ManagerSetting { managerSettingPort :: Int
                                     , managerSettingHost :: !String}
                    deriving Show


-- Manager API
-- default ManageSetting configuration paramater
defaultManagerSetting = ManagerSetting 4043 "localhost"

-- socket, connection stream, key abstraction.
data Manager = Manager { mgrConns :: [Int]
                        ,mgrPeer  :: Int
                        ,mgrSock  :: S.Socket
                        -- hash from 4 tuple to socket
                       }
             deriving Show

-- newManager generate `Manager`.
newManager :: ManagerSetting -> IO Manager
newManager = return undefined

-- closeManager close `Manager`.
closeManager :: Manager -> IO ()
closeManager mgr = undefined

-- withManager take care action IO a.
withManager :: Manager -> IO a -> IO [Frame]
withManager = undefined

-- Header

-- Header is header format.
-- while reconnection or handshake, we should  use long Header format.
-- otherwise, we must use short Header format.


errorCodeToInt :: ErrorCode -> Int
errorCodeToInt (ApplicationErrorCode i) = i
errorCodeToInt (HostLocalErrorCode i)   = i
errorCodeToInt (QUICError err)          = quicErrorToInt err
errorCodeToInt (CrypotograhicError i)   = i

intToErrorCode :: Int -> ErrorCode
intToErrorCode i
 | (0x00000000 <= i && i <= 0x3FFFFFFF) = ApplicationErrorCode i
 | (0x40000000 <= i && i <= 0x7FFFFFFF) = HostLocalErrorCode i
 | (0x80000000 <= i && i <= 0xBFFFFFFF) = QUICErrorCode (intToQUICError i)
 | (0xC0000000 <= i && i <= 0xFFFFFFFF) = CrypotograhicError i

quicErrorToInt :: QUICError -> Int
quicErrorToInt ApplicationErrorCode i = i
quicErrorToInt HostLocalErrorCode i   = i
quicErrorToInt QUICErrorCode e        = quicErrorToInt e
quicErrorToInt CrypotograhicError i   = i

intToQUICError :: Int -> QUICError
intToQUICError i = f (i - 0x80000000)
  where
    f :: Int -> QUICError
    f 0x01 = QUICInternalError
    f 0x02 = QUICStreamDataAfterTermination
    f 0x03 = QUICInvalidPacketHeader
    f 0x04 = QUICInvalidFrameData
    f 0x05 = QUICMultipleTerminationOffsets
    f 0x06 = QUICStreamCancelled
    f 0x07 = QUICClosedCriticalStream
    f 0x30 = QUICMissingPayload
    f 0x2e = QUICInvalidStreamData
    f 0x3d = QUICUnencryptedStreamData
    f 0x59 = QUICMaybeCorruptedMemory
    f 0x06 = QUICInvalidRstStreamData
    f 0x07 = QUICInvalidConnectionCloseData
    f 0x08 = QUICInvalidGoawayData
    f 0x39 = QUICInvalidWindowUpdateData
    f 0x3a = QUICInvalidBlockedData
    f 0x4e = QUICInvalidPathCloseData
    f 0x09 = QUICInvalidAckData
    f 0x0a = QUICInvalidVersionNegotiationPacket
    f 0x0b = QUICInvalidPublicReset
    f 0x0c = QUICDecryptionFailure
    f 0x0d = QUICEncryptionFailure
    f 0x0e = QUICPacketTooLarge
    f 0x10 = QUICPeerGoingAway
    f 0x11 = QUICInvalidStreamId
    f 0x31 = QUICInvalidPriority
    f 0x12 = QUICTooManyOpenStreams
    f 0x4c = QUICTooManyAvailableStreams
    f 0x13 = QUICPublicReset
    f 0x14 = QUICInvalidVersion
    f 0x16 = QUICInvalidHeaderId
    f 0x17 = QUICInvalidNegotiationValue
    f 0x18 = QUICDecompressionFailure
    f 0x19 = QUICNetworkIdleTimeout
    f 0x43 = QUICHandshakeTimeout
    f 0x2a = QUICErrorMigratingAddress
    f 0x56 = QUICErrorMigratingPort
    f 0x32 = QUICEmptyStreamFrameNoFin
    f 0x3b = QUICFlowControlRecievedTooMuchData
    f 0x3f = QUICFlowControlSentTooMuchData
    f 0x40 = QUICFlowControlInvalidWindow
    f 0x3e = QUICConnectionIpPooled
    f 0x44 = QUICTooManyOutstandingSentPackets
    f 0x45 = QUICTooManyOutstandingRecievedPackets
    f 0x46 = QUICConnectionCancelled
    f 0x47 = QUICBadPacketLossRate
    f 0x49 = QUICPublicResetPostHandshake
    f 0x4a = QUICTimeoutsWithOpenStreams
    f 0x55 = QUICTooManyRTOs
    f 0x2c = QUICEncryptionLevelIncorrect
    f 0x37 = QUICVersionNegotiationMissmatch
    f 0x50 = QUICIpAddressChanged
    f 0x51 = QUICAddressValidationFailure
    f 0x5d = QUICTooManyFrameGaps
    f 0x60 = QUICTooManySessionsOnServers
    f _    = error "there are some conflict erro code"



-- TODO: porting from draft code like c++.  It should be replaced more suitable data format.
-- TODO: it demands to adapt TLS. they are requred TLS extension "quic_transport_paramters".


data TransportParameterId = TransParamInitialMaxStreamData
                          | TransParamInitialMaxData
                          | TransParamInitialMaxStreamId
                          | TransParamIdleTimeout
                          | TransParamTruncateConnectionId
                          | TransParamMaxPacketSize
                          | TransParamUnknown Int
                          deriving Show

data TransportParameter = TransportParameter {
                          transParamId    :: TransportParameterId
                        , transParamValue :: ByteString

                        }
                        deriving Show

data TransportParameters = TransportParametersClientHello QUICVersion QUICVersion [TransportParameter]
                         | TransportPArametersEncryptedExtensions [QUICVersion] [TransportParameter]
                         deriving Show

-- entity of transport parameters.
-- TODO: the config will be replaced hash map structure.
-- key: the digit , value: numerical value 0x000 ~ 0xfeff
data Config = Config { configMaxStreamData :: Int
            , configMaxData                :: Int
            , configMaxStreamId            :: StreamId
            , configTimeout                :: QUICTime
            , configTruncateConnectionId   :: ConnectionId
            , configMaxPacketSize          :: Int}
            deriving Show

upperLimitMaxPacketSize :: Int -> Bool
upperLimitMaxPacketSize i = i < 65527

lowerLimitMaxPacketSize :: Int -> Bool
lowerLimitMaxPacketSize i = i > 1252


-- the number that library support protocol version.
supportedVersion :: [Int32]
supportedVersion = [0x0d,
                    0x0e
                   ]

-- latest number that the libary support
defaultVersion :: Int32
defaultVersion = 0x0e

data Mode = Client | Server
          deriving Show

-- Internaly Manager
data EndPoint = EndPoint { endPointMode :: Mode
              }
              deriving Show


-- TODO: QUIC WG discuss to simplify Stream State.
-- it should obey the result of discussion.
data Handle = Idle | Open | RemoteHalfClose | LocalHalfClose | Closed
            deriving Show


-- QUIC time format

parseTime :: ByteString -> QUICTime
parseTime bs = undefined
  where
    mantissa = undefined
    exponent = undefined

toUTCTime :: QUICTime -> Clock.UTCTime
toUTCTime = undefined

fromUTCTime :: Clock.UTCTime -> QUICTime
fromUTCTime = undefined

-- QUIC Version  Negtiaton

isVaildVersion :: QUICVersion -> Bool
isVaildVersion = undefined

isSupportedversion :: QUICVersion -> Bool
isSupportedversion = undefined

supportedVersions :: [QUICVersion] -> [QUICVersion]
supportedVersions = undefined

-- Init STREAM
-- STREAM ID 0
-- padded 1280 frame.

initStream = undefined

newStreamFrame :: Frame
newStreamFrame = undefined


-- Obtain Stream Data

waitData :: StreamId -> ByteString
waitData = undefined

-- Stream Data Internaly

insertStreamData = undefined

-- Stream Data Map , Key: Stream Id / Value: (Packet Number, Data)
-- the data is specific Stream Id Data

type StreamData = M.Map Offset ByteString

type DataMap = M.Map StreamId StreamData

initDataMap :: DataMap
initDataMap = M.empty

insertDataMap :: DataMap
              -> StreamId
              -> Offset
              -> ByteString
              -> DataMap
insertDataMap m sid ofs bs = modify' sid ofs bs sd m
    where
      sd = lookup' sid m
      lookup' ::  StreamId -> DataMap -> (Maybe StreamData)
      lookup' sid' m' = M.lookup sid' m'

      modify' :: StreamId -> Offset -> ByteString -> (Maybe StreamData) -> DataMap -> DataMap
      modify' sid' ofs' bs' sd0 m' = M.insert sid' sd1 m'
        where
          sd1 = case sd0 of
                    (Just sd) ->  M.insert ofs' bs' sd
                    Nothing   ->  M.insert ofs' bs' M.empty

dropDataMap :: StreamId -> DataMap -> DataMap
dropDataMap sid m = M.delete sid m

takeDataMap :: StreamId
            -> DataMap
            -> (DataMap, Maybe ByteString)
takeDataMap sid m = case (M.lookup sid m) of
                      (Just sd) -> (M.delete sid m, Just $ toBS sd)
                      (Nothing) -> (m, Nothing)
                where
                  toBS :: StreamData -> ByteString
                  toBS sd = foldl BS.append BS.empty $ map snd $ M.toList sd
                  f :: ByteString -> (PacketNumber, ByteString) -> ByteString
                  f b (k,v) = b `BS.append` v


-- TODO: having our own the handshake procedure is bad practice.
-- We should carry TLS handshake from Network.TLS (tls package)
-- but api is nothing in tls package. So we have implementation for now.
-- it will be replaced.

sendHandshake1RTT = undefined
  where
  -- send Client Hello with 1RTT
  sendClientHello = undefined

  -- recv Server Hello with 1RTT
  recvServerHello = undefined


recvHandshake = undefined
  where
  -- recv Client Hello with 1RTT
  recvClinetHello = undefined

  sendServerHello = undefined
--
-- testing
--
testDataMap = undefined
testStreamData = undefined
