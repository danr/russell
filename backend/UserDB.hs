{-# LANGUAGE RecordWildCards #-}
module UserDB where

import Control.Applicative

import Data.Text.Lazy (Text)
import Control.Concurrent.STM
import Data.Map (Map)
import qualified Data.Map as M

import Data.Maybe

data User = User
    { user_score :: Int
    , user_words :: Int
    , user_history :: [(Text,Int)]
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

emptyUser :: User
emptyUser = User
    { user_score = 0
    , user_words = 0
    , user_history = []
    }

