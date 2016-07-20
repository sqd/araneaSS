{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Crypto (
    decryptAES256CFB,
    encryptAES256CFB
    ) where

import Data.Conduit

import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder
import Data.Conduit.Binary as CB

import qualified Data.Sequence as S
import Data.Foldable
import Data.List as L

import OpenSSL
import OpenSSL.EVP.Cipher
import OpenSSL.EVP.Internal
import qualified Crypto.Hash.MD5 as MD5

import System.Random

import Control.Monad
import Control.Monad.Trans

import Control.Exception

import Common

type SeqBS = S.Seq BS.ByteString

bytesToKey :: BS.ByteString -> Int -> Int -> (BS.ByteString, BS.ByteString)
bytesToKey password keyLen ivLen = (key, iv) where
    loopRst = fst $ until (not . pred) loop (S.empty, 0)
    ms = BS.concat $ toList loopRst
    (key, rest) = BS.splitAt keyLen ms
    iv = BS.take ivLen rest

    pred :: (SeqBS, Int) -> Bool
    pred (m, i) = (sum $ fmap BS.length m) < keyLen + ivLen

    loop :: (SeqBS, Int) -> (SeqBS, Int)
    loop (m, i) = (newm, newi) where
        dat = if i > 0
              then BS.concat [m `S.index` (i - 1), password]
              else password
        newm = m S.|> (MD5.hash dat)
        newi = i + 1

prepareCipher :: MonadIO m
    => String
    -> Password
    -> ByteString
    -> CryptoMode
    -> m (Cipher, CipherCtx)
prepareCipher cipher key iv mode = liftIO $ withOpenSSL $ do
    cipher <- getCipherByName cipher >>= \case
        Just c -> return c
        Nothing -> error ("No cipher for " ++ cipher)
    ctx <- cipherInitBS cipher key iv mode
    return (cipher, ctx)

decryptAES256CFB :: Password -> Conduit ByteString IO ByteString
decryptAES256CFB rawKey = do
    iv <- fmap LBS.toStrict $ CB.take 16
    let (key, _) = bytesToKey rawKey 32 16

    (aesCipher, cipherContext) <- prepareCipher "AES-256-CFB" key iv Decrypt

    awaitForever $ \d ->
        if BS.null d
        then yield ""
        else (liftIO $ withOpenSSL $ cipherUpdateBS cipherContext d) >>= yield

encryptAES256CFB :: Password -> Conduit ByteString IO ByteString
encryptAES256CFB rawKey = do
    rndGen <- liftIO newStdGen
    let iv = pack $ L.take 16 $ randoms rndGen
        (key, _) = bytesToKey rawKey 32 16
    yield iv

    (aesCipher, cipherContext) <- prepareCipher "AES-256-CFB" key iv Encrypt

    awaitForever $ \d ->
        (liftIO $ withOpenSSL $ cipherUpdateBS cipherContext d) >>= yield
