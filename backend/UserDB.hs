{-# LANGUAGE RecordWildCards #-}
module UserDB where

import Control.Applicative

import Data.Text.Lazy (Text)
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe

import Websocket

data User = User
    { user_sink    :: Sink
    , user_score   :: Int
    , user_words   :: Int
    , user_history :: [(Text,Int)]
    }

instance Show User where
    show User{..} = show (user_score,user_words,user_history)

type UserDB = Map String User

emptyUserDB :: UserDB
emptyUserDB = M.empty

userPlaced :: String -> Text -> TVar UserDB -> STM Bool
userPlaced name word db
    = maybe False ((word `elem`) . map fst . user_history)
    . M.lookup name <$> readTVar db

userMod :: Sink -> String -> (Int -> Int) -> ([(Text,Int)] -> [(Text,Int)])
        -> TVar UserDB -> STM (Int,Int)
userMod sink name mod_score mod_history db = do
    user@User{..} <- fromMaybe (emptyUser sink) . M.lookup name <$> readTVar db
    let history' = mod_history user_history
        words' = length history'
        score' = mod_score user_score
        updated = user
            { user_score = score'
            , user_words = words'
            , user_history = history'
            }
    modifyTVar db (M.insert name updated)
    return (score',words')

resetScores :: TVar UserDB -> STM ()
resetScores v = do
    udb <- readTVar v
    writeTVar v (M.map (emptyUser . user_sink) udb)

emptyUser :: Sink -> User
emptyUser sink = User
    { user_sink    = sink
    , user_score   = 0
    , user_words   = 0
    , user_history = []
    }

