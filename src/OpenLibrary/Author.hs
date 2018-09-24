{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}


module OpenLibrary.Author where


import Protolude


import Data.Aeson
import GHC.Generics


data Author
  = Author
  { name :: Text
  , url :: Text
  } deriving (Generic, Show)


instance FromJSON Author
instance ToJSON Author


