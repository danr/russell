{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving, ViewPatterns #-}
module Main where

import Prelude hiding (words)
import Data.Monoid (mconcat)

import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty

import Data.Aeson hiding (json)
import GHC.Generics

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

type Coord = (Int,Int)
type Snake = [Coord]

submitSnake :: Submit -> Snake
submitSnake = map (\[x,y] -> (x,y)) . snake

ok :: Snake -> Bool
ok _ = True

main :: IO ()
main = scotty 3000 $ do

    middleware logStdout
    middleware $ staticPolicy $ mconcat
        [ addBase "frontend"
        , noDots
        , foldr1 (<|>) (map hasSuffix
            ["html", "js", "css", "jpg", "txt"])
        ] <|> only (zip ["","/"] (repeat "frontend/index.html"))


    post "/submit" $ do
        Just submit <- jsonData
        json $ Response
            { correct = ok (submitSnake submit)
            , score   = 100
            , words   = 1
            }

