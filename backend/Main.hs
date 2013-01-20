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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Read as T

import System.Environment
import Control.Concurrent
import Data.Time.Clock

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
    , resp_timeout :: Integer
    }
  deriving Generic

instance ToJSON Response

data GridResponse = GridResponse
    { grid_response    :: Grid
    , grid_char_scores :: [(Char,Int)]
    , grid_timeout     :: Integer
    }
  deriving Generic

data SummaryResponse = SummaryResponse
    { summary_scores  :: [(String,User)]
    , summary_timeout :: Integer
    }
  deriving Generic

instance ToJSON SummaryResponse
instance ToJSON GridResponse

submitSnake :: Submit -> Snake
submitSnake = map (\[x,y] -> (x,y)) . snake

type Snake = [Coord]

data User = User
    { user_score :: Int
    , user_words :: Int
    , user_history :: [(Text,Int)]
    }
  deriving Generic

instance ToJSON User

emptyUser :: User
emptyUser = User
    { user_score = 0
    , user_words = 0
    , user_history = []
    }

type UserDB = Map String User

emptyUserDB :: UserDB
emptyUserDB = M.empty

userPlaced :: String -> Text -> TVar UserDB -> STM Bool
userPlaced name word db
    = maybe False ((word `elem`) . map fst . user_history)
    . M.lookup name <$> readTVar db

userMod :: String -> (Int -> Int) -> ([(Text,Int)] -> [(Text,Int)])
        -> TVar UserDB -> STM (Int,Int)
userMod name mod_score mod_history db = do
    User{..} <- fromMaybe emptyUser . M.lookup name <$> readTVar db
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

readLexicon :: IO (HashSet Text)
readLexicon = HS.fromList . T.lines <$> T.readFile "backend/saldom-stripped"

readTrigrams :: IO Trigrams
readTrigrams = HM.fromList . map frequency . T.lines <$>
    T.readFile "backend/saldom-trigram-count"
  where
    frequency :: Text -> (Text,Int)
    frequency l = (T.tail w,either error fst (T.decimal n))
      where (n,w) = T.break (== ' ') l

us :: Int
us = 1000 * 1000

play_length :: Integer
play_length = 120

score_length :: Integer
score_length = 20

diffToMs :: NominalDiffTime -> Integer
diffToMs = truncate . (precision *) . toRational
  where precision = 1000

main :: IO ()
main = do

    args <- getArgs

    lexicon <- readLexicon

    trigrams <- readTrigrams

    when (args == ["test"]) $
        print $ map (`HS.member` lexicon) ["FEST","VAG","ANOR","ROR","ABCD"]

    when (args == ["grids"]) $ forever $ do
        g <- makeGrid trigrams
        T.putStrLn ""
        mapM_ T.putStrLn g

    grid_var <- newTVarIO Nothing
    scores_var <- newTVarIO []

    next_change <- newTVarIO =<< getCurrentTime

    db <- newTVarIO emptyUserDB

    let play_mode_stm = maybe False (const True) <$> readTVar grid_var

        play_mode = atomically play_mode_stm

        calc_next_change = do
            next <- atomically (readTVar next_change)
            t <- getCurrentTime
            let ms = diffToMs (diffUTCTime next t)
            print $ "ms to next change: " ++ show ms
            return ms

    forkIO $ forever $ do
        p <- play_mode
        let delay = if p then play_length else score_length
        t0 <- getCurrentTime
        let next = addUTCTime (fromInteger delay) t0
        atomically $ writeTVar next_change next
        putStrLn $ "Play mode: " ++ show p
        threadDelay (fromInteger delay * 1000000)
        case p of
            -- Go from play to score
            True -> atomically $ do
                writeTVar grid_var Nothing
                scores <- M.toList <$> readTVar db
                writeTVar scores_var scores
            -- Go from score to play
            False -> do
                g <- makeGrid trigrams
                atomically $ do
                    writeTVar grid_var (Just g)
                    writeTVar db emptyUserDB

    scotty 3000 $ do

        middleware logStdout
        middleware $ staticPolicy $ mconcat
            [ addBase "frontend"
            , noDots
            , foldr1 (<|>) (map hasSuffix
                ["html", "js", "css", "jpg", "txt"])
            ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

        get "/grid" $ do
            g <- liftIO $ atomically $ do
                mg <- readTVar grid_var
                case mg of
                    Nothing -> retry
                    Just g' -> return g'
            nc <- liftIO $ calc_next_change
            json $ GridResponse
                { grid_response    = g
                , grid_char_scores = M.toList char_scores
                , grid_timeout     = nc
                }

        get "/summary" $ do
            scores <- liftIO $ atomically $ do
                pm <- play_mode_stm
                when pm retry
                readTVar scores_var
            nc <- liftIO calc_next_change
            json $ SummaryResponse
                { summary_scores  = scores
                , summary_timeout = nc
                }

        post "/submit" $ do
            mg <- liftIO $ atomically $ readTVar grid_var
            case mg of
                Nothing -> json ()
                Just grid -> do
                    Just submit <- jsonData
                    let word = map (grid `at`) (submitSnake submit)
                        value = sum (map (char_scores M.!) word)
                        word_text = T.pack word
                    res <- liftIO $ do
                        (ok,(score',words')) <- atomically $ do
                            let name = user submit
                            user_placed <- userPlaced name word_text db
                            let word_ok = word_text `HS.member` lexicon
                                ok = not user_placed && word_ok
                                (mod_score,mod_history)
                                    | ok        = ((+ value),((word_text,value):))
                                    | otherwise = (id,id)
                            (,) ok <$> userMod name mod_score mod_history db
                        nc <- calc_next_change
                        return $ Response
                            { correct      = ok
                            , score        = score'
                            , words        = words'
                            , resp_timeout = nc
                            }
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

