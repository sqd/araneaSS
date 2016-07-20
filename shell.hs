{-# LANGUAGE OverloadedStrings #-}

import Data.Conduit
import Data.Conduit.Network

import Control.Monad.Trans
import Control.Concurrent.Async

import Data.Streaming.Network.Internal (HostPreference(Host))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import Data.List.Split

import System.Environment

import Crypto

printC :: Sink BS.ByteString IO ()
printC = awaitForever $ \d ->
    liftIO $ print d

main = do
    args <- getArgs
    (addr, port, p) <- return $ case args of
            [x, y] ->
                if ':' `elem` x
                then let [addr, port] = splitOn ":" x
                      in (addr, read port, y)
                else (x, 7001, y)
            _ -> error "usage: shell IP:port password"
    let pwd = BS8.pack p
    BS.putStr "> "
    s <- BS.getLine
    runTCPClient (clientSettings port $ BS8.pack addr) $ \server -> do
        let cmd = BS.concat [s, ";"]
            serverSink = encryptAES256CFB pwd =$= appSink server
            serverSource = appSource server =$= decryptAES256CFB pwd
        concurrently (yield cmd $$ serverSink) (serverSource $$ printC)
    main
