{-# LANGUAGE DeriveGeneric #-}
module ServerProtocol where

import Data.Aeson
import GHC.Generics
import Data.Text.Lazy (Text)

data ServerProtocol
    = InvalidUsername
    | Connected   { username :: String }
    | Response    { correct :: Bool, score :: Int }
    | ScoreBoard  { timeout :: Integer, scores :: [(String,Int,Int)] }
    | FinalScores { timeout :: Integer, final_scores :: [(String,[(Text,Int)])] }
    | Grid        { timeout :: Integer, grid :: [Text], char_scores :: [(Char,Int)] }
  deriving (Show,Generic)

instance ToJSON ServerProtocol

