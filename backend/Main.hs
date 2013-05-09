{-# LANGUAGE RecordWildCards #-}
module Main where

import Prelude hiding (words)
import Data.Monoid (mconcat)

import Control.Monad
import Control.Concurrent.STM

import Network.Wai.Middleware.Static
import Web.Scotty hiding (next)

import Network.WebSockets as WS

import Control.Concurrent
import Data.Time.Clock

import UserDB
import Lexicon
import Game
import RusselMonad
import Websocket

main :: IO ()
main = do

    lexicon <- readLexicon

    trigrams <- readTrigrams

    current_time <- getCurrentTime

    state <- newTVarIO (Scoreboard current_time)

    user_db <- newTVarIO emptyUserDB

    next_grid <- newEmptyTMVarIO

    let run_russel = runRusselM RusselEnv{..}

    void $ forkIO $ run_russel gridMaker

    void $ forkIO $ run_russel game

    void $ forkIO $ run_russel notify

    -- Websocket server
    void $ forkIO $ runServer "0.0.0.0" 8000 $
        handleRequest run_russel loginHandler handleError

    -- Web server
    scotty 3000 $ middleware $ staticPolicy $ mconcat

        [ addBase "frontend"
        , noDots
        , foldr1 (<|>) (map hasSuffix
            ["html", "js", "css", "jpg", "txt"])
        ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

