{-# LANGUAGE DeriveGeneric #-}
module ClientProtocol where

import Data.Aeson
import GHC.Generics

data ClientProtocol
    = Connect { username :: String }
    | Send    { message :: String }
  deriving (Show,Generic)

instance FromJSON ClientProtocol

