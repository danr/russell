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
import Control.Concurrent.STM

import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty

import Data.Aeson hiding (json)
import GHC.Generics

import Data.Maybe

import Network.WebSockets as WS
-- import Network

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

import Control.Concurrent
import Data.Time.Clock

import UserDB
import Lexicon
import Grid
import ServerProtocol
import ClientProtocol

lexiconFile :: FilePath
lexiconFile = "backend/saldom-stripped"

trigramsFile :: FilePath
trigramsFile = "backend/saldom-trigram-count"

play_length :: Integer
play_length = 120

score_length :: Integer
score_length = 12

us :: Int
us = 1000 * 1000

diffToMs :: NominalDiffTime -> Integer
diffToMs = truncate . (precision *) . toRational
  where precision = 1000

main :: IO ()
main = do

    lexicon <- readLexicon lexiconFile

    trigrams <- readTrigrams trigramsFile

    state <- newTVarIO Nothing

    let grid_var      = fmap fst <$> readTVar state
        notify_thread = fmap snd <$> readTVar state

    scores_var <- newTVarIO []

    next_change <- newTVarIO =<< getCurrentTime

    db <- newTVarIO emptyUserDB
    sinks <- newTVarIO M.empty

    let -- Handle login from the user
        login sink = forever $ do
            msg <- receiveJSON
            case msg of
                Just Connect{..} -> do
                    liftIO $ do
                        atomically $ do
                            modifyTVar sinks (M.insert username sink)
                            modifyTVar db (M.insert username emptyUser)
                        sendJSON sink (Connected username)
                        flip withJust (sendJSON sink) =<< makeGridMsg
                        sendJSON sink =<< makeScoreMsg
                    listen sink username
                _ -> return ()

        -- Handle snake submit messages
        listen sink name = forever $ do
            msg <- receiveJSON
            mg <- liftIO $ atomically grid_var
            liftIO $ case (msg,mg) of
                (Just Submit{..},Just grid) -> (sendJSON sink =<<) $
                    case snakeOnGrid lexicon grid charScores snake of
                        Nothing -> return Response { correct = False, score = 0 }
                        Just (word_text,value) -> do
                            ok <- atomically $ do
                                ok <- not <$> userPlaced name word_text db
                                let (mod_score,mod_history)
                                        | ok        = ((+ value),((word_text,value):))
                                        | otherwise = (id,id)
                                userMod name mod_score mod_history db
                                return ok
                            return Response
                                { correct = ok
                                , score = if ok then value else 0
                                }
                _ -> return ()

        -- Handle errors
        handleErr sink e = liftIO $ do
            putStrLn $ "Got an error: " ++ show e
            atomically $ modifyTVar sinks (M.filter (/= sink))

        -- Send message to all
        sendToAll :: ToJSON a => a -> IO ()
        sendToAll m = do
            sinks' <- atomically (readTVar sinks)
            mapM_ (`sendJSON` m) (M.elems sinks')

        -- Make the score board message
        makeScoreMsg = do
            (mg,users) <- atomically $ liftM2 (,) grid_var (readTVar db)
            timeout <- calc_next_change
            case mg of
                Just grid -> do
                    let scores =
                            [ (u,user_score,user_words)
                            | (u,User{..}) <- M.toList users
                            ]
                    return (ScoreBoard timeout scores)
                Nothing -> do
                    let scores =
                            [ (u,user_score,user_history)
                            | (u,User{..}) <- M.toList users
                            ]
                    return (FinalScores timeout scores)

        -- Make the grid message
        makeGridMsg = do
            mg <- atomically grid_var
            return $ fmap (\ grid -> Grid grid (M.toList charScores)) mg

        play_mode = maybe False (const True) <$> grid_var

        calc_next_change = do
            next <- atomically (readTVar next_change)
            t <- getCurrentTime
            let ms = diffToMs (diffUTCTime next t)
            print $ "ms to next change: " ++ show ms
            return ms

    forkIO $ forever $ do
        p <- atomically play_mode
        let delay = if p then play_length else score_length
        t0 <- getCurrentTime
        let next = addUTCTime (fromInteger delay) t0
        atomically $ writeTVar next_change next
        putStrLn $ "Play mode: " ++ show p
        threadDelay (fromInteger delay * 1000000)
        case p of
            -- Go from play to score
            True -> do
                thrd <- atomically $ do
                    writeTVar state Nothing
                    scores <- M.toList <$> readTVar db
                    writeTVar scores_var scores
                    notify_thread
                withJust thrd killThread
                sendToAll =<< makeScoreMsg
            -- Go from score to play
            False -> do
                g <- makeGrid trigrams
                thrd <- forkIO $ forever $ do
                    sendToAll =<< makeScoreMsg
                    threadDelay (1000 * 1000)
                atomically $ do
                    writeTVar state (Just (g,thrd))
                    -- Change this so all logged in users start with 0 score instead
                    writeTVar db emptyUserDB
                (`withJust` sendToAll) =<< makeGridMsg

    -- Web server
    forkIO $ scotty 3000 $ do

        middleware logStdout
        middleware $ staticPolicy $ mconcat
            [ addBase "frontend"
            , noDots
            , foldr1 (<|>) (map hasSuffix
                ["html", "js", "css", "jpg", "txt"])
            ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

    -- Websocket server
    runServer "0.0.0.0" 8000 $ \ rq -> do

        WS.acceptRequest rq
        WS.spawnPingThread 1
        liftIO . putStrLn . ("Client version: " ++) =<< WS.getVersion
        sink <- WS.getSink
        login sink `catchWsError` handleErr sink

receiveJSON :: FromJSON a => WebSockets Hybi10 (Maybe a)
receiveJSON = fmap decode WS.receiveData

sendJSON :: ToJSON a => Sink Hybi10 -> a -> IO ()
sendJSON s = WS.sendSink s . DataMessage . Text . encode

withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust (Just m) f = f m
withJust Nothing  _ = return ()

charScores :: Map Char Int
charScores = M.fromList
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

