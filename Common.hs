module Common (
    toLazyBS,
    getWord,
    c2w,
    w2c,
    getCommand,
    handleAny,
    catchAny,
    ignoreError,
    always,
    LBS.toStrict,
    Password,
    Address,
    Port
    ) where

import Data.ByteString as BS
import Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import Data.Word8

import Data.Attoparsec.ByteString as A

import Data.Conduit

import Control.Exception

type Password = ByteString
type Address = ByteString
type Port = Int

toLazyBS :: ByteString -> LBS.ByteString
toLazyBS = toLazyByteString . byteString

getWord :: Parser ByteString
getWord = do
    let pred = \x -> isSpace x || x == c2w ';'
    A.takeWhile pred
    takeTill pred

getCommand :: Parser [ByteString]
getCommand = do
    next <- peekWord8'
    if next == c2w ';'
    then anyWord8 >> return []
    else do
        skipWhile isSpace
        w <- takeTill $ \x -> isSpace x || x == c2w ';'
        if not $ BS.null w
        then fmap (w:) getCommand
        else return []

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

handleAny :: (SomeException -> IO a) -> IO a -> IO a
handleAny = handle

ignoreError :: IO () -> IO ()
ignoreError = handleAny $ const $ return ()

always :: IO () -> IO () -> IO ()
always e a = handleAny (const e) a
