{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, ViewPatterns #-}
module Main where

import Prelude hiding (words)
import Data.Monoid (mconcat)

import Control.Monad.IO.Class (liftIO)

{-
import Control.Monad.Random
import Control.Monad.Random.Class
-}
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty

import Data.Aeson hiding (json)
import GHC.Generics

import Data.Map (Map)
import qualified Data.Map as M

data Submit = Submit
    { snake :: [[Int]]
    , user  :: String
    }
  deriving Generic

instance FromJSON Submit

data Response = Response
    { correct :: Bool
    , score   :: Int
    , words   :: Int
    }
  deriving Generic

instance ToJSON Response

data Round = Round
    { round_grid        :: [String]
    , round_char_scores :: [(Char,Int)]
    }
  deriving Generic

instance ToJSON Round

submitSnake :: Submit -> Snake
submitSnake = map (\[x,y] -> (x,y)) . snake

type Coord = (Int,Int)
type Snake = [Coord]

data User = User
    { user_score :: Int
    , user_words :: Int
    , user_history :: [String]
    }

ok :: Snake -> Bool
ok _ = True

main :: IO ()
main = scotty 3000 $ do

    let char_scores =
            [ ('A',1)
            , ('B',4)
            , ('C',8)
            , ('D',1)
            , ('E',1)
            , ('F',4)
            , ('G',2)
            , ('H',3)
            , ('I',1)
            , ('J',8)
            , ('K',3)
            , ('L',1)
            , ('M',3)
            , ('N',1)
            , ('O',2)
            , ('P',3)
            , ('Q',10)
            , ('R',1)
            , ('S',1)
            , ('T',1)
            , ('U',3)
            , ('V',4)
            , ('X',10)
            , ('Y',8)
            , ('Z',10)
            , ('Å',4)
            , ('Ä',4)
            , ('Ö',4)
            ]

        grid =
            [ "ACKS"
            , "RLIA"
            , "ÄOTR"
            , "NHIE"
            ]

        round = Round { round_grid = grid , round_char_scores = char_scores }

    middleware logStdout
    middleware $ staticPolicy $ mconcat
        [ addBase "frontend"
        , noDots
        , foldr1 (<|>) (map hasSuffix
            ["html", "js", "css", "jpg", "txt"])
        ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

    get "/round" $ json round

    post "/submit" $ do
        b <- body
        liftIO (print b)
        Just submit <- jsonData
        json $ Response
            { correct = ok (submitSnake submit)
            , score   = 100
            , words   = 1
            }

