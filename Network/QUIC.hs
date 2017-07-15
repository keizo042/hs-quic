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

intToErrorCode :: Int -> ErrorCode
intToErrorCode _ = undefined

errorCodeToInt :: ErrorCode -> Int
errorCodeToInt (ApplicationErrorCode i) = i
errorCodeToInt (HostLocalErrorCode i)   = i
errorCodeToInt (QUICError err)          = quicErrorToInt err
errorCodeToInt (CrypotograhicError i)   = i

quicErrorToInt :: QUICError -> Int
quicErrorToInt i
 | (0x00000000 <= i && i <= 0x3FFFFFFF) = ApplicationErrorCode i
 | (0x40000000 <= i && i <= 0x7FFFFFFF) = HostLocalErrorCode i
 | (0x80000000 <= i && i <= 0xBFFFFFFF) = QUICErrorCode (intToQUICError i)
 | (0xC0000000 <= i && i <= 0xFFFFFFFF) = CrypotograhicError i


intToQUICError :: Int -> QUICError
intToQUICError = undefined

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

-- Prioritazation is stream prioritazation abstraction
data Priority = Priority {}
                    deriving Show

defaultPrioritazation :: Priority
defaultPrioritazation = undefined

incrPriority :: Priority
incrPriority = undefined

decrPriority :: Priority
decrPriority = undefined

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
