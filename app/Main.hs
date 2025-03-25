{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main where

import Control.Concurrent.Async (Concurrently (..))
import Control.Exception (Exception)
import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO)
import Data.Map qualified as Map
import Data.String.Conversions (convertString)
import Data.Text (Text)
import Data.Text.Lazy qualified as LazyText
import Data.Typeable (Typeable)
import GHC.IO.Handle.Types (Handle)
import Log (exitWithErrorMessage, logInfo, printLnToStderr)
import Network.HTTP.Types (forbidden403, internalServerError500, notFound404)
import Network.Wai.Handler.Warp as Warp (defaultSettings, setPort)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Network.Wai.Middleware.Static qualified as Static
import Static (hashFiles)
import System.Environment.MrEnv (envAsBool, envAsInt)
import System.Process (ProcessHandle, createProcess, proc)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 ((!))
import Text.Blaze.Html5 qualified as H
import Text.Blaze.Html5.Attributes qualified as A
import Web.Scotty.Trans qualified as S
import Prelude hiding (readFile, writeFile)


-- Main

main :: IO ()
main = do
    cfg <- getConfig "ROCHA_"
    printLnToStderr $ show cfg
    logInfo $ "Running webserver on port " ++ show cfg.port
    _ <-
        runConcurrently $
            (,,)
                <$> Concurrently (runWebserver cfg)
                <*> Concurrently (when cfg.debug $ void buildFrontend)
    return ()


-- | In debug mode (aka dev mode), also run the frontenv dev server.
buildFrontend :: IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
buildFrontend =
    npm "install" >> npm "start"
    where
        npm arg = createProcess $ proc "npm" [arg]


-- Config

type StaticFiles = Map.Map FilePath Text


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
    hashFiles staticFileInput >>= \case
        Left err ->
            -- Couldn't get hashed files. Exit fatally.
            exitWithErrorMessage $ show err
        Right hashedFiles ->
            -- Got hashed files. Run the webserver.
            let
                staticFiles :: Map.Map FilePath Text
                staticFiles = Map.map convertString hashedFiles
            in
                S.scottyOptsT opts id (webserver cfg{staticFiles = staticFiles})
    where
        -- In prod mode, also hash the generated CSS and JS files for
        -- cache-busting reasons.
        newCfg :: Config
        newCfg =
            if cfg.debug
                then cfg
                else cfg{staticFilesSources = cfg.staticFilesSources ++ ["static/main.css", "static/main.js"]}

        staticFileInput :: [FilePath]
        staticFileInput = map convertString newCfg.staticFilesSources

        opts :: S.Options
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
        S.throw NotFound


-- Pages

page :: StaticFiles -> Text -> Text -> H.Html -> LazyText.Text
page staticFiles bodyClass title content =
    let
        css :: Text
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
            "Christian is currently the founder and CEO of "
                <> (H.a ! A.href "https://charm.sh/" $ "Charm")
                <> "."
        H.p "Christian was born in Los Angeles and live and works in New York."
        H.ul ! A.class_ "contact" $ do
            H.li $ H.a ! A.href "https://linkedin.com/in/meowgorithm" $ "LinkedIn"
            H.li $ H.a ! A.href "https://github.com/meowgorithm" $ "GitHub"
            H.li $ H.a ! A.href "https://x.com/meowgorithm" $ "X"
        H.p $ H.text $ "Copyright © " <> convertString (toRoman 2025) <> " Christian Rocha"


-- Helpers

-- Convert an integer to Roman numerals.
toRoman :: Int -> String
toRoman n = f n numerals
    where
        numerals =
            [ (1000, "M")
            , (900, "CM")
            , (500, "D")
            , (400, "CD")
            , (100, "C")
            , (90, "XC")
            , (50, "L")
            , (40, "XL")
            , (10, "X")
            , (9, "IX")
            , (5, "V")
            , (4, "IV")
            , (1, "I")
            ]
        f 0 _ = ""
        f n' ((value, numeral) : rest)
            | n' >= value = numeral ++ f (n' - value) ((value, numeral) : rest)
            | otherwise = f n' rest
        f _ [] = ""
