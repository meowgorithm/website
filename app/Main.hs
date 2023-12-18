{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent.Async (Concurrently (..))
import Control.Exception (Exception, try)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Function ((&))
import Data.Map qualified as Map
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Typeable (Typeable)
import GHC.IO.Handle.Types (Handle)
import Main.Utf8 (withUtf8)
import Network.HTTP.Types (forbidden403, internalServerError500, notFound404)
import Network.Wai.Handler.Warp as Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static qualified as Static
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment.MrEnv (envAsBool, envAsInt)
import System.Exit (exitFailure)
import System.FilePath (joinPath, takeBaseName, takeExtension)
import System.Process (ProcessHandle, createProcess, proc)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Scotty.Trans qualified as S
import Prelude hiding (readFile, writeFile)


-- Main

main :: IO ()
main = withUtf8 $ do
    cfg <- getConfig "ROCHA_"
    print cfg
    logInfo $ "もしもし? Running webserver on port " ++ show cfg.port
    _ <-
        runConcurrently $
            (,,)
                <$> Concurrently (runWebserver cfg)
                <*> Concurrently (when cfg.debug $ void buildFrontend)
    return ()


-- | In debug mode (aka dev mode), run the frontenv dev server.
buildFrontend :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
buildFrontend =
    createProcess (proc "npm" ["install"])
        >> createProcess (proc "npm" ["start"])


-- Config

data Config = Config
    { port :: Int
    , debug :: Bool
    , staticDir :: Text
    , staticFilesSources :: [Text]
    , staticFiles :: StaticFiles
    }


instance Show Config where
    show (Config port debug staticDir _ _) =
        "\nConfig:\n  port: "
            <> show port
            <> "\n  debug: "
            <> show debug
            <> "\n  staticDir: "
            <> show staticDir
            <> "\n"


getConfig :: String -> IO Config
getConfig prefix =
    Config
        <$> envAsInt (prefix <> "PORT") 8000
        <*> envAsBool (prefix <> "DEBUG") False
        <*> pure "static"
        <*> pure []
        <*> pure Map.empty


-- Static Files

type StaticFiles = Map.Map Text Text


-- | Hash static files and write them to the static file directory.
hashStaticFiles :: Config -> IO StaticFiles
hashStaticFiles cfg =
    mapM (hashFile cfg.staticDir) cfg.staticFilesSources >>= \hashes ->
        pure $ Map.fromList $ zip cfg.staticFilesSources hashes


{-| Hash a file and write the file to disk with the hash in the filename.
 Errors fatally if a file cannot be hashed.
-}
hashFile :: Text -> Text -> IO Text
hashFile outputPath pathToFile =
    logInfo ("Hashing " <> path <> "...")
        >> tryReading path
        >>= \case
            Left except ->
                exitWithErrorMessage ("Error reading file " <> path <> ": " <> show except)
            Right bytes ->
                getHashedFilename bytes >>= \hashedFilename ->
                    doesFileExist hashedFilename >>= \case
                        True ->
                            logInfo ("Already hashed: " <> path <> " -> " <> hashedFilename)
                                >> pure (convertString hashedFilename)
                        False ->
                            tryWriting hashedFilename bytes >>= \case
                                Left except ->
                                    exitWithErrorMessage ("Error hashing file: " <> path <> ": " <> show except)
                                Right _ ->
                                    logInfo ("Hashed: " <> path <> " -> " <> hashedFilename)
                                        >> pure (convertString hashedFilename)
    where
        path :: String
        path = convertString pathToFile

        staticDir :: String
        staticDir = convertString outputPath

        tryReading :: String -> IO (Either IOError ByteString)
        tryReading path' = try (readFile path')

        tryWriting :: String -> ByteString -> IO (Either IOError ())
        tryWriting path' bytes =
            createDirectoryIfMissing True staticDir
                >> try (writeFile path' bytes)

        getHashedFilename :: ByteString -> IO FilePath
        getHashedFilename bytes =
            shortHash bytes >>= \hash ->
                pure $
                    joinPath
                        [ staticDir
                        , takeBaseName path <> "." <> hash <> takeExtension path
                        ]


-- | Calculate a (truncated) SHA256 hash for a given byte string.
shortHash :: (Applicative f) => ByteString -> f String
shortHash bytes =
    (hashlazy bytes :: Digest SHA256)
        & show
        & take 7
        & pure


-- Webserver

-- | HTTP exceptions
data Except
    = Forbidden
    | NotFound
    | Error String
    deriving (Show, Eq, Typeable, Exception)


-- | HTTP exception handling.
handleException :: (MonadIO m) => S.ErrorHandler m
handleException = S.Handler $ \case
    Forbidden -> S.status forbidden403 >> S.html forbiddenPage
    NotFound -> S.status notFound404 >> S.html notFoundPage
    Error _ -> S.status internalServerError500 >> S.html internalServerErrorPage


runWebserver :: Config -> IO ()
runWebserver cfg =
    hashStaticFiles newCfg >>= \staticFiles ->
        S.scottyOptsT opts id (webserver cfg{staticFiles = staticFiles})
    where
        -- In prod mode, also hash the generated CSS and JS files for
        -- cache-busting reasons.
        newCfg =
            if cfg.debug
                then cfg
                else cfg{staticFilesSources = cfg.staticFilesSources ++ ["static/main.css", "static/main.js"]}

        opts =
            S.Options
                { S.verbose = 0 -- disable star trek output
                , S.settings = Warp.setPort cfg.port Warp.defaultSettings
                }


webserver :: Config -> S.ScottyT IO ()
webserver cfg = do
    S.middleware (if cfg.debug then logStdoutDev else logStdout)
    S.middleware Static.static
    S.defaultHandler handleException

    S.get "/" $ do
        S.html $ homePage cfg.staticFiles

    S.notFound $
        S.raiseStatus notFound404 "Not Found"


-- Pages

page :: StaticFiles -> Text -> Text -> H.Html -> LazyText.Text
page staticFiles bodyClass title content =
    let
        css = Map.findWithDefault "/static/main.css" "static/main.css" staticFiles
        js = Map.findWithDefault "/static/main.js" "static/main.js" staticFiles
    in
        renderHtml $ H.docTypeHtml $ do
            H.head $ do
                H.meta ! A.charset "utf-8"
                H.meta ! A.name "viewport" ! A.content "width=device-width,initial-scale=1,user-scalable=no"
                H.link ! A.rel "stylesheet" ! A.href (H.textValue css)
                H.title $ H.text title
            H.body ! bodyAttrs $ do
                content
                H.script "" ! A.src (H.textValue js)
    where
        bodyAttrs =
            if bodyClass == ""
                then mempty
                else A.class_ (H.textValue bodyClass)


notFoundPage :: LazyText.Text
notFoundPage =
    page Map.empty "error" "404 Not Found" $ do
        H.h1 "404 Not Found"
        H.p "The resource you requested was not found on this server."


forbiddenPage :: LazyText.Text
forbiddenPage =
    page Map.empty "error" "403 Forbidden" $ do
        H.h1 "403 Forbidden"
        H.p "You are not allowed to taste the forbidden fruit."


internalServerErrorPage :: LazyText.Text
internalServerErrorPage =
    page Map.empty "error" "500 Internal Server Error" $ do
        H.h1 "500 Internal Server Error"
        H.p "Something went wrong."


homePage :: StaticFiles -> LazyText.Text
homePage staticFiles =
    page staticFiles "" "Hello" $ do
        H.h1 "Christian Rocha"
        H.p $
            "Christian is an "
                <> H.em "homme d’affaires"
                <> " and creative "
                <> H.em "tour de force"
                <> " who blurs the lines between business, art, design, and technology. "
                <> "He believes these disciplines go hand in hand by the nature of the practice."
        H.p "He has founded two venture-backed companies, built very popular open source software, and created brands with cult-like followings."
        H.p $
            "Christian is currently a co-founder of "
                <> (H.a ! A.href "https://charm.sh/" $ "Charm")
                <> "."
        H.p "Christian was born in Los Angeles and live and works in New York."
        H.ul ! A.class_ "contact" $ do
            H.li $ H.a ! A.href "https://linkedin.com/in/meowgorithm" $ "LinkedIn"
            H.li $ H.a ! A.href "https://github.com/meowgorithm" $ "GitHub"
            H.li $ H.a ! A.href "https://x.com/meowgorithm" $ "X/Twitter"


-- Helpers

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage =
    (>> exitFailure) . putStrLn . ("[FATAL] " <>)


logInfo :: String -> IO ()
logInfo =
    putStrLn . ("[INFO] " <>)


logWarn :: String -> IO ()
logWarn =
    putStrLn . ("[WARN] " <>)
