{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}

module Manager (
    ManagerConfig (..),
    defaultConfig,
    runManager
    ) where

import Control.Monad
import Control.Monad.Trans

import Data.Conduit
import Data.Conduit.Attoparsec
import Data.Conduit.Combinators as CC
import Data.Conduit.Network
import Data.Streaming.Network
import Data.Streaming.Network.Internal (HostPreference(Host))
import Network.Socket

import System.Log.Logger

import System.Exit

import Data.Maybe
import Data.List as L
import Data.List.Split

import qualified Data.IntMap as M

import Control.Concurrent (forkIO, ThreadId, killThread)
import Control.Concurrent.MVar

import Data.ByteString as BS
import Data.ByteString.Char8 as BS8

import Control.Exception

import Connection
import Common
import Crypto
import Stats
import Test


data ManagerResource = ManagerResource {
    stats :: Stats,
    threads :: MVar (M.IntMap ThreadId)
    }

data ManagerConfig = ManagerConfig {
    managerPassword :: String,
    managerAddress :: String,
    managerPort :: Port,
    trustIPs :: Maybe [String]
    }

defaultConfig :: ManagerConfig
defaultConfig = ManagerConfig {
    managerPassword = "",
    managerAddress = "0.0.0.0",
    managerPort = 7001,
    trustIPs = Nothing
    }

handleManager :: ManagerResource -> Consumer ByteString IO ByteString
handleManager res = do
    cmd <- sinkParser $ getCommand
    let (action:args) = cmd
    liftIO $ case action of
        "ping" -> return "pong"
        "add" -> do
            mp <- newEmptyMVar
            let [pwd] = args
                serverSetting = setAfterBind (\s -> socketPort s >>= putMVar mp)
                    $ serverSettings 0 "*"
            tid <- forkIO $ ignoreError $ runTCPServer serverSetting $ \c -> do
                port <- fmap fromIntegral $ readMVar $ mp
                relayTCP c pwd $ newStatsC port $ stats res
            port <- fmap fromIntegral $ readMVar $ mp
            modifyMVar_ (threads res) $ return . M.insert port tid
            modifyMVar_ (stats res) $ return . M.insert port 0
            infoM "Manager" ("Start a TCP server at " ++ show port)
            return $ BS8.pack $ show port
        "remove" -> do
            let [p] = args
                port = read $ BS8.unpack p
            ts <- takeMVar $ threads res
            if port `M.notMember` ts
            then return "not existed"
            else do
                killThread $ ts M.! port
                modifyMVar_ (stats res) $ return . M.delete port
                return (M.delete port ts, "ok")
                infoM "Manager" ("Remove the TCP server at " ++ show port)
                putMVar (threads res) $ M.delete port ts
                return "ok"
        "stats" -> fmap (BS8.pack . show) $ getStats $ stats res
        "clear" -> clearStats (stats res) >> return "ok"
        "test" -> do
            let [server, p, pwd, target] = args
                port = read $ BS8.unpack p
            resp <- testServer server port pwd target
            return $ case resp of
                Left _ -> "error"
                Right s -> BS8.pack $ show s

relayManager :: AppData -> Password -> ManagerResource -> IO ()
relayManager client pwd res = do
    resp <- appSource client =$= decryptAES256CFB pwd $$ handleManager res
    yield resp =$= encryptAES256CFB pwd $$ appSink client

runManager :: ManagerConfig -> IO ()
runManager conf = withSocketsDo $ do
    let (addr, port) = (managerAddress conf, managerPort conf)
        managerSetting = serverSettings port $ Host addr
    stats <- newStats
    threads <- newMVar M.empty
    let res = ManagerResource {stats = stats, threads = threads}
    updateGlobalLogger "Manager" $ setLevel INFO
    infoM "Manager" ("Manager listening at " ++ addr ++ ":" ++ show port)
    runTCPServer managerSetting $ \client -> do
        let strClientAddr = show $ appSockAddr client
            [clientAddr, clientPort] = splitOn ":" strClientAddr
        let pwd = BS8.pack $ managerPassword conf
        always (infoM "Manager" $ "Bad request from" ++ strClientAddr) $ case trustIPs conf of
            Just list -> if clientAddr `L.elem` list
                         then do
                             noticeM "Manager" ("Manager from " ++ strClientAddr)
                             relayManager client pwd res
                         else warningM "Manager" ("Untrusted manager from " ++ strClientAddr)
            Nothing -> do
                noticeM "Manager" ("Accept command from " ++ strClientAddr)
                relayManager client pwd res
