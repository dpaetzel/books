{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}


-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json


module Main where


import Protolude


import Data.Aeson
import Data.Aeson.Types
import Data.Text
import qualified Data.Text.Lazy.Encoding as LE
import Data.Map
import Control.Applicative
import Control.Arrow (left)
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import GHC.Generics


import Author


type ISBN = Text


data Book
  = Book
  { authors :: [Author]
  , isbn :: ISBN
  , title :: Text
  , url :: Text
  }
  deriving (Show)


fromOpenLibrary :: ISBN -> B.ByteString -> Either Text Book
fromOpenLibrary isbn s
  = left pack $ do
    result <- eitherDecode s
    flip parseEither result $ \obj -> do
      obj' <- obj .: ("ISBN:" <> isbn)
      authors <- obj' .: "authors"
      title <- obj' .: "title"
      url <- obj' .: "url"
      return $ Book authors isbn title url


openLibraryURL :: ISBN -> Text
openLibraryURL isbn
  = "https://openlibrary.org/api/books?format=json&jscmd=data&bibkeys=ISBN:" <> isbn


book
  :: (ISBN -> B.ByteString -> Either Text Book)
  -> (ISBN -> Text)
  -> ISBN
  -> IO (Either Text Book)
book parseIt createUrl isbn
  = fmap (parseIt isbn) . simpleHttp . unpack . createUrl $ isbn


main :: IO ()
main = do
 d <- book fromOpenLibrary openLibraryURL "9780980200447"
 case d of
  Left err -> putStrLn err
  Right ps -> print ps
