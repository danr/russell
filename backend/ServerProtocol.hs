{-# LANGUAGE DeriveGeneric,TypeSynonymInstances,FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module ServerProtocol where

import Data.Aeson
import GHC.Generics
import Data.Text (Text)
import Data.Array

import Grid

data ServerProtocol
    = InvalidUsername
    | Connected   { username :: String }
    | Response    { correct :: Bool, score :: Int }
    | ScoreBoard  { timeout :: Integer, scores :: [(String,Int,Int)] }
    | FinalScores { timeout :: Integer, final_scores :: [(String,[(Text,Int)])] }
    | Grid        { timeout :: Integer, grid :: Grid, char_scores :: [(Char,Int)] }
  deriving (Show,Generic)

instance ToJSON ServerProtocol

instance ToJSON Grid where
    toJSON a = toJSON [ [ a ! (x,y) | x <- [x0..x1] ] | y <- [y0..y1] ]
      where
        ((x0,y0),(x1,y1)) = bounds a

