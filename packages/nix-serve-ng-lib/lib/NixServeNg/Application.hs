{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module NixServeNg.Application (
  ApplicationOptions (..),
  makeApplication,
  validHashPart,
  validHashPartBytes,
) where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.CharSet.ByteSet (ByteSet (..))
import Network.Wai (Application)
import Nix (NoSuchPath (..), PathInfo (..))

import Control.Exception qualified as Control.Exception
import Control.Monad qualified as Monad
import Control.Monad.Except qualified as Except
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.CharSet.ByteSet qualified as ByteSet
import Data.Vector qualified as Vector
import Data.Void qualified as Void
import Network.HTTP.Types qualified as Types
import Network.Wai qualified as Wai
import Nix qualified

data ApplicationOptions = ApplicationOptions
  { priority :: Integer
  , storeDirectory :: ByteString
  , secretKey :: Maybe ByteString
  }

-- https://github.com/NixOS/nix/blob/2.8.1/src/libutil/hash.cc#L83-L84
validHashPartBytes :: ByteSet
validHashPartBytes =
  ByteSet.fromList
    ( [0x30 .. 0x39] -- 0..9
        <> [0x61 .. 0x64] -- abcd
        <> [0x66 .. 0x6E] -- fghijklmn
        <> [0x70 .. 0x73] -- pqrs
        <> [0x76 .. 0x7A] -- vwxyz
    )

validHashPart :: ByteString -> Bool
validHashPart hash = ByteString.all (`ByteSet.member` validHashPartBytes) hash

makeApplication :: ApplicationOptions -> Application
makeApplication ApplicationOptions {..} request respond = do
  let stripStore = ByteString.stripPrefix (storeDirectory <> "/")

  let done = Except.throwError

  let internalError message = do
        let headers = [("Content-Type", "text/plain")]

        let builder = "Internal server error: " <> message <> ".\n"

        let response =
              Wai.responseBuilder
                Types.status500
                headers
                builder

        done response

  let noSuchPath = do
        let headers = [("Content-Type", "text/plain")]

        let builder = "No such path.\n"

        let response =
              Wai.responseBuilder
                Types.status404
                headers
                builder

        done response

  let invalidPath = do
        let headers = [("Content-Type", "text/plain")]

        let builder = "Invalid path.\n"

        let response =
              Wai.responseBuilder
                Types.status400
                headers
                builder

        done response

  result <- Except.runExceptT do
    let rawPath = Wai.rawPathInfo request

    if
      | Just prefix <- ByteString.stripSuffix ".narinfo" rawPath
      , Just hashPart <- ByteString.stripPrefix "/" prefix -> do
          Monad.unless (ByteString.length hashPart == 32 && validHashPart hashPart) do
            invalidPath

          maybeStorePath <- liftIO (Nix.queryPathFromHashPart hashPart)

          storePath <- case maybeStorePath of
            Left NoSuchPath -> noSuchPath
            Right storePath -> return storePath

          pathInfo@PathInfo {..} <- liftIO (Nix.queryPathInfo storePath)

          narHash2 <- case ByteString.stripPrefix "sha256:" narHash of
            Nothing -> do
              internalError "NAR hash missing sha256: prefix"
            Just narHash2 -> do
              return narHash2

          referenceNames <- case traverse stripStore references of
            Nothing -> do
              internalError "references missing store directory prefix"
            Just names -> do
              return names

          let referencesBuilder
                | not (Vector.null referenceNames) =
                    "References:"
                      <> foldMap (\name -> " " <> Builder.byteString name) referenceNames
                      <> "\n"
                | otherwise =
                    mempty

          deriverBuilder <-
            case deriver of
              Just d ->
                case stripStore d of
                  Just name ->
                    return
                      ( "Deriver: "
                          <> Builder.byteString name
                          <> "\n"
                      )
                  Nothing -> do
                    internalError "deriver missing store directory prefix"
              Nothing ->
                return mempty

          fingerprint <- case Nix.fingerprintPath storePath pathInfo of
            Nothing -> internalError "invalid NAR hash"
            Just builder -> do
              return (ByteString.Lazy.toStrict (Builder.toLazyByteString builder))

          signatures <- case secretKey of
            Just key -> do
              signature <- liftIO (Nix.signString key fingerprint)

              return (Vector.singleton signature)
            Nothing -> do
              return sigs

          let buildSignature signature =
                "Sig: " <> Builder.byteString signature <> "\n"

          let builder =
                "StorePath: "
                  <> Builder.byteString storePath
                  <> "\nURL: nar/"
                  <> Builder.byteString hashPart
                  <> "-"
                  <> Builder.byteString narHash2
                  <> ".nar\nCompression: none\nNarHash: "
                  <> Builder.byteString narHash
                  <> "\nNarSize: "
                  <> Builder.word64Dec narSize
                  <> "\n"
                  <> referencesBuilder
                  <> deriverBuilder
                  <> foldMap buildSignature signatures

          let size =
                ( ByteString.Lazy.toStrict
                    . Builder.toLazyByteString
                    . Builder.int64Dec
                    . ByteString.Lazy.length
                    . Builder.toLazyByteString
                )
                  builder

          let headers =
                [ ("Content-Type", "text/x-nix-narinfo")
                , ("Content-Length", size)
                ]

          let response =
                Wai.responseBuilder
                  Types.status200
                  headers
                  builder

          done response
      | Just prefix <- ByteString.stripSuffix ".nar" rawPath
      , Just interior <- ByteString.stripPrefix "/nar/" prefix -> do
          let interiorLength = ByteString.length interior

          (hashPart, maybeExpectedNarHash) <-
            if
              | interiorLength == 85
              , (hashPart, rest) <- ByteString.splitAt 32 interior
              , Just (0x2D, expectedNarHash) <- ByteString.uncons rest -> do
                  return (hashPart, Just (ByteString.Char8.pack "sha256:" <> expectedNarHash))
              | interiorLength == 32 -> do
                  return (interior, Nothing)
              | otherwise -> do
                  invalidPath

          Monad.unless (validHashPart hashPart) do
            invalidPath

          maybeStorePath <- liftIO (Nix.queryPathFromHashPart hashPart)

          storePath <- case maybeStorePath of
            Left NoSuchPath -> noSuchPath
            Right storePath -> return storePath

          PathInfo {narHash} <- liftIO (Nix.queryPathInfo storePath)

          Monad.unless (all (narHash ==) maybeExpectedNarHash) do
            let headers = [("Content-Type", "text/plain")]

            let builder =
                  "Incorrect NAR hash. Maybe the path has been recreated.\n"

            let response =
                  Wai.responseBuilder
                    Types.status404
                    headers
                    builder

            done response

          let streamingBody write flush = do
                result <- Nix.dumpPath hashPart callback

                case result of
                  Left exception -> Control.Exception.throwIO exception
                  Right x -> return x
                where
                  callback builder = do
                    () <- write builder
                    flush

          let headers = [("Content-Type", "text/plain")]

          let response =
                Wai.responseStream Types.status200 headers streamingBody

          done response
      | Just suffix <- ByteString.stripPrefix "/log/" rawPath -> do
          let hashPart = ByteString.take 32 suffix

          Monad.unless (ByteString.length hashPart == 32 && validHashPart hashPart) do
            invalidPath

          maybeBytes <- liftIO (Nix.dumpLog suffix)

          bytes <- case maybeBytes of
            Nothing -> noSuchPath
            Just bytes -> return bytes

          let lazyBytes = ByteString.Lazy.fromStrict bytes

          let headers = [("Content-Type", "text/plain")]

          let response =
                Wai.responseLBS Types.status200 headers lazyBytes

          done response
      | rawPath == "/nix-cache-info" -> do
          let headers = [("Content-Type", "text/plain")]

          let builder =
                "StoreDir: "
                  <> Builder.byteString storeDirectory
                  <> "\nWantMassQuery: 1\nPriority: "
                  <> Builder.integerDec priority
                  <> "\n"

          let response =
                Wai.responseBuilder Types.status200 headers builder

          done response
      | otherwise -> do
          let headers = [("Content-Type", "text/plain")]

          let builder = "File not found.\n"

          let response =
                Wai.responseBuilder Types.status404 headers builder

          done response

  case result of
    Left response -> respond response
    Right void -> Void.absurd void
