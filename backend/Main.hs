{-# LANGUAGE OverloadedStrings, DeriveGeneric, GeneralizedNewtypeDeriving,
             ViewPatterns, RecordWildCards #-}
module Main where

import Prelude hiding (words)
import Data.Monoid (mconcat)

import Control.Monad.IO.Class (liftIO)

import Control.Applicative hiding ((<|>))
import Control.Monad.Random
import Control.Monad.Random.Class
import Control.Monad
import Control.Monad.STM
import Control.Concurrent.STM.TVar

import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty

import Data.Aeson hiding (json)
import GHC.Generics

import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.HashSet (HashSet)
import qualified Data.HashSet as S

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Grid

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
    { round_grid        :: Grid
    , round_char_scores :: [(Char,Int)]
    }
  deriving Generic

instance ToJSON Round

submitSnake :: Submit -> Snake
submitSnake = map (\[x,y] -> (x,y)) . snake

type Snake = [Coord]

data User = User
    { user_score :: Int
    , user_words :: Int
    , user_history :: [String]
    }

emptyUser :: User
emptyUser = User
    { user_score = 0
    , user_words = 0
    , user_history = []
    }

type UserDB = Map String User

newUserDB :: IO (TVar UserDB)
newUserDB = newTVarIO M.empty

probmap :: Map Int Char
probmap = M.fromList $ zip [1..]
    $ concatMap (\(c,v) -> replicate (550 - v * v * v) c)
    $ M.toList char_scores

{-
newGrid :: IO Grid
newGrid = replicateM 4 $ replicateM 4
        $ (probmap M.!) `fmap` getRandomR (0 :: Int,M.size probmap)
        -}

userPlaced :: String -> String -> TVar UserDB -> STM Bool
userPlaced name word db = (maybe False ((word `elem`) . user_history)
                        . M.lookup name) `fmap` readTVar db

userMod :: String -> (Int -> Int) -> ([String] -> [String])
        -> TVar UserDB -> STM (Int,Int)
userMod name mod_score mod_history db = do
    User{..} <- (fromMaybe emptyUser . M.lookup name) `fmap` readTVar db
    let history' = mod_history user_history
        words' = length history'
        score' = mod_score user_score
        updated = User
            { user_score = score'
            , user_words = words'
            , user_history = history'
            }
    modifyTVar db (M.insert name updated)
    return (score',words')

readSaldo :: IO [Text]
readSaldo
    = filter ((>1) . T.length) . map T.toUpper . T.lines
   <$> T.readFile "backend/saldom-stripped"

readSaldoWordsAndSet :: IO ([Text],HashSet Text)
readSaldoWordsAndSet = do

    saldo_list <- readSaldo

    putStrLn $ "Read " ++ show (length saldo_list) ++ " words."

    let ok_len x = 5 <= x && x <= 9
        good_words = filter (ok_len . T.length) (saldo_list)

    let saldo_set = S.fromList saldo_list

    return (good_words,saldo_set)

main :: IO ()
main = do

    (good_words,saldo_set) <- readSaldoWordsAndSet

    print $ map (`S.member` saldo_set)
            ["FEST","VAG","ANOR","ROR","ABCD"]

    grid <- makeGrid good_words

    print grid

    let round = Round
            { round_grid = grid
            , round_char_scores = M.toList char_scores
            }

    db <- newUserDB

    scotty 3000 $ do

        middleware logStdout
        middleware $ staticPolicy $ mconcat
            [ addBase "frontend"
            , noDots
            , foldr1 (<|>) (map hasSuffix
                ["html", "js", "css", "jpg", "txt"])
            ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

        get "/round" $ json round

        post "/submit" $ do
            Just submit <- jsonData
            let word = map (grid `at`) (submitSnake submit)
                value = sum (map (char_scores M.!) word)
            res <- liftIO $ atomically $ do
                let name = user submit
                user_placed <- userPlaced name word db
                let word_ok = S.member (T.pack word) saldo_set
                    ok = not user_placed && word_ok
                    (mod_score,mod_history)
                        | ok        = ((+ value),(word:))
                        | otherwise = (id,id)
                uncurry (Response ok) `fmap`
                    userMod name mod_score mod_history db
            json res

char_scores :: Map Char Int
char_scores = M.fromList
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

