{-# LANGUAGE DeriveGeneric #-}
module ClientProtocol where

import Data.Aeson
import GHC.Generics

data ClientProtocol
    = Connect { username :: String }
    | Submit  { snake    :: [[Int]] }
  deriving (Show,Generic)

instance FromJSON ClientProtocol

