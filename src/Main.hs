{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}


-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json


module Main where


import Protolude


import Control.Arrow (left)
import Control.Concurrent (threadDelay)
import Data.Text
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import System.IO (BufferMode(NoBuffering), hSetBuffering)


import Book
import Goodreads
import Goodreads.APIKey
import OpenLibrary
import Abebooks


fetch
  :: (ISBN -> B.ByteString -> Either Text Book)
  -> (ISBN -> Text)
  -> ISBN
  -> IO (Either (Text, ISBN) Book)
fetch parseIt createUrl isbn
  = fmap (left (, isbn) . parseIt isbn) . simpleHttp . unpack . createUrl $ isbn


fetchAll :: ISBN -> IO (Either (Text, ISBN) Book)
fetchAll isbn
  = do
    threadDelay 2000000
    b <- fetch fromOpenLibrary openLibraryURL isbn
    case b of
      Left _ -> do
        b <- fetch fromGoodreads (goodreadsURL apiKey) isbn
        case b of
          Left _ -> fetch fromAbebooks abebooksURL isbn
          Right b -> return . Right $ b
      Right b -> return . Right $ b


test = "9783442472444" :: Text


main
  = do
    (file : _) <- getArgs
    contents <- readFile file
    sequence $ fetchAndSave file <$> lines contents
  where
    fetchAndSave file isbn
      = do
        bookE <- fetchAll isbn
        case bookE of
          Left (_, isbn) ->
            hPutStrLn stderr isbn
          Right book -> do
            hSetBuffering stdout NoBuffering
            hPutStrLn stdout . csv $ book
