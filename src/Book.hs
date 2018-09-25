{-# LANGUAGE NoImplicitPrelude #-}


module Book where


import Protolude


type ISBN = Text


data Book
  = Book
  { authors :: [Text]
  , isbn :: ISBN
  , title :: Text
  , source :: Text
  }
  deriving (Show)


