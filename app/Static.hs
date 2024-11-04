{-# LANGUAGE LambdaCase #-}

module Static (hashFiles, HashedFiles) where

import Control.Exception (try)
import Control.Monad (foldM)
import Crypto.Hash (Digest, SHA256, hashlazy)
import Data.ByteString.Lazy (ByteString, readFile, writeFile)
import Data.Function ((&))
import Data.Map qualified as Map
import Log (logInfo)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath (joinPath, takeBaseName, takeDirectory, takeExtension)
import Prelude hiding (readFile, writeFile)


-- | A map of original file paths to hashed file paths.
type HashedFiles = Map.Map FilePath FilePath


{-| Hash the contents of a list of files and write the files to disk with the
hash in the filename.
-}
hashFiles :: [FilePath] -> IO (Either IOError HashedFiles)
hashFiles = foldM processFile (Right Map.empty)
    where
        processFile (Left err) _ = pure (Left err)
        processFile (Right acc) path =
            getHashAndWriteFile path >>= \result ->
                pure $ result >>= \hash -> Right $ Map.insert path hash acc


-- | Hash a file and write the file to disk with the hash in the filename.
getHashAndWriteFile :: FilePath -> IO (Either IOError FilePath)
getHashAndWriteFile path =
    logInfo ("Hashing " <> path <> "...")
        >> tryReading path
        >>= \case
            Left except ->
                pure (Left except)
            Right bytes ->
                getHashedFilename bytes >>= \hashedFilename ->
                    doesFileExist hashedFilename >>= \case
                        True ->
                            -- Already processed. Nothing to do.
                            logInfo ("Already hashed: " <> path <> " -> " <> hashedFilename)
                                >> pure (Right hashedFilename)
                        False ->
                            -- Not processed yet. Write the file.
                            tryWriting hashedFilename bytes >>= \case
                                Left except ->
                                    pure $ Left except
                                Right _ ->
                                    logInfo ("Hashed: " <> path <> " -> " <> hashedFilename)
                                        >> pure (Right hashedFilename)
    where
        staticDir :: FilePath
        staticDir = takeDirectory path

        tryReading :: FilePath -> IO (Either IOError ByteString)
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

        -- \| Calculate a (truncated) SHA256 hash for a given byte string.
        shortHash :: (Applicative f) => ByteString -> f String
        shortHash bytes =
            (hashlazy bytes :: Digest SHA256)
                & show
                & take 7
                & pure
