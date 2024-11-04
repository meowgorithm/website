module Log (exitWithErrorMessage, logInfo, logWarn, printLnToStderr) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)


exitWithErrorMessage :: String -> IO a
exitWithErrorMessage =
    (>> exitFailure) . printLnToStderr . ("[FATAL] " <>)


logInfo :: String -> IO ()
logInfo =
    printLnToStderr . ("[INFO] " <>)


logWarn :: String -> IO ()
logWarn =
    printLnToStderr . ("[WARN] " <>)


printLnToStderr :: String -> IO ()
printLnToStderr =
    hPutStrLn stderr
