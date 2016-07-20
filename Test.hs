{-# LANGUAGE OverloadedStrings #-}

module Test (
    testServer
    ) where

import Data.Conduit
import Data.Conduit.Binary as CB
import Data.Conduit.Network

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as BS8

import Data.Binary.Put

import Network.BSD

import Control.Exception

import Common
import Crypto


httpRequest :: ByteString -> ByteString
httpRequest host = BS.concat ["GET / HTTP/1.1\r\n", "Host: ", host, "\r\n\r\n"]

testServer :: Address
           -> Port
           -> Password
           -> Address
           -> IO (Either SomeException Bool)
testServer server port pwd target = try $ do
    q <- getHostByName $ BS8.unpack target
    let w32Addr = hostAddress q
        bsAddr = toStrict $ runPut $ putWord32le w32Addr
        clientSetting = clientSettings port server

    runTCPClient clientSetting $ \server -> do
        yield (BS.concat ["\1", bsAddr, "\0\80", httpRequest target])
                =$= encryptAES256CFB pwd $$ appSink server
        resp <- appSource server =$= decryptAES256CFB pwd $$ CB.take 4
        return (resp == "HTTP")
