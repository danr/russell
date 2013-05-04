{-# LANGUAGE DeriveGeneric #-}
module ServerProtocol where

import Data.Aeson
import GHC.Generics

data ServerProtocol
    = InvalidUsername
    | Connected { username :: String }
    | Broadcast { username :: String, message :: String }
    | Userlist  { usernames :: [String] }
  deriving (Show,Generic)

instance ToJSON ServerProtocol

