{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Connection (
    relayTCP
    ) where

import Data.Conduit
import Data.Conduit.Network
import Control.Concurrent.Async

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8
import Data.Binary.Get
import Data.Word

import Data.Attoparsec.ByteString as A
import Data.Conduit.Attoparsec

import Data.Char
import Data.Maybe
import Data.List as L

import Control.Monad
import Control.Monad.Trans

import Control.Exception

import Common
import Crypto
import Stats


bsToPort :: ByteString -> Int
bsToPort = fromIntegral . runGet getWord16be . toLazyBS

bsToIPv4 :: ByteString -> Address
bsToIPv4 x = flip runGet (toLazyBS x) $ do
    fields <- replicateM 4 getWord8
    return $ BS.intercalate "." $ L.map (BS8.pack . show) fields

bsToIPv6 :: ByteString -> Address
bsToIPv6 s = undefined

consumeConnectionRequest :: Consumer ByteString IO (Address, Port)
consumeConnectionRequest = sinkParser $ do
    addrType <- satisfy $ (`L.elem` [1, 3, 4])
    address <- case addrType of
        0x1 -> fmap bsToIPv4 $ A.take 4
        0x3 -> do
            lenAddr <- fmap fromIntegral anyWord8
            A.take lenAddr
        0x4 -> fmap bsToIPv6 $ A.take 16
    port <- fmap bsToPort $ A.take 2
    return (address, port)

relayTCP :: AppData -> Password -> StatsC -> IO ()
relayTCP client pwd statsc = void $ do
    let clientSink = statsc =$= (encryptAES256CFB pwd) =$= appSink client
        clientSource = appSource client =$= (decryptAES256CFB pwd) =$= statsc
    (resumeSource, (address, port)) <- clientSource $$+ consumeConnectionRequest
    let clientSetting = clientSettings port address
    runTCPClient clientSetting $ \server -> concurrently
            (resumeSource $$+- appSink server)
            (appSource server $$ clientSink)
