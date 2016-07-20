{-# LANGUAGE LambdaCase #-}
module CmdOption (
    CmdOption,
    getOption,
    getConfig,
    checkConfig
    ) where

import Control.Monad

import System.Environment
import System.Console.GetOpt
import System.Exit

import Data.List.Split

import Manager

-- TODO: make password neccesary
-- TODO: check options


data CmdOption =
    Version
    | Usage
    | Password String
    | ManagerAddr String
    | TrustIP String

options :: [OptDescr CmdOption]
options = [
    Option ['v'] [] (NoArg Version) "show version number.",
    Option ['h'] [] (NoArg Usage) "show usage.",
    Option ['k'] [] (ReqArg Password "[PASSWORD]") "manager password.",
    Option ['s'] [] (ReqArg ManagerAddr "[ADDR:PORT]") "manager address. default: 0.0.0.0:7001.",
    Option ['t'] [] (ReqArg TrustIP "[IP1,IP2..IPn]") "trust only directions from these IPs. default: trust all."
    ]

header :: String
header = "AraneaSS: a crowd-sharing SS service.\nhttp://github.com/sqd/AraneaSS\n"

appendOption :: ManagerConfig -> CmdOption -> IO ManagerConfig
appendOption conf opt =
    case opt of
        Version -> do
            putStrLn "v0.1"
            exitSuccess
        Usage -> do
            putStr $ usageInfo header options
            exitSuccess
        Password x -> return $ conf {managerPassword = x}
        TrustIP x -> return $ conf {trustIPs = Just $ x `splitOn` ","}
        ManagerAddr x -> return $ conf {managerAddress = addr, managerPort = port}
            where
                (addr, port)
                    | (f:b:_) <- ":" `splitOn` x = (f, read b)
                    | otherwise = (x, 7001)

getOption :: IO ([CmdOption], [String], [String])
getOption = do
    args <- getArgs
    return $ getOpt Permute options args

getConfig :: ([CmdOption], [String], [String]) -> IO ManagerConfig
getConfig = \case
    ([], [], []) -> do
        putStr $ usageInfo header options
        exitSuccess
    (flags, [], []) -> foldM appendOption defaultConfig flags
    (_, noOpts, []) -> do
        putStrLn $ "unrecognized options: " ++ unwords noOpts
        exitFailure
    (_, _, noArgs) -> do
        putStrLn $ "bad arguments:" ++ concat noArgs ++ usageInfo header options
        exitFailure

checkConfig :: ManagerConfig -> IO ()
checkConfig conf = do
    when (null $ managerPassword conf) $ do
        putStrLn "Must set a password."
        exitFailure
    when (trustIPs conf == Just ["127.0.0.1"]) $ putStrLn "Only trust 127.0.0.1. Are you sure?"
