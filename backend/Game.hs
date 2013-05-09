{-# LANGUAGE RecordWildCards,NamedFieldPuns #-}
module Game where

import Control.Concurrent.STM hiding (always)
import Control.Concurrent

import Control.Applicative hiding ((<|>))
import Control.Monad
import Control.Monad.Reader

import GHC.Exception

import Data.Map (Map)
import qualified Data.Map as M

import Data.Time.Clock

import ServerProtocol
import ClientProtocol
import Websocket
import UserDB
import Grid
import RusselMonad

playLength :: Integer
playLength = 120 * us

scoreLength :: Integer
scoreLength = 12 * us

game :: RusselM ()
game = forever $ do
    RusselEnv{state,user_db,next_grid} <- ask
    delay <- nextChange
    s <- liftIO $ do
        threadDelay (fromInteger delay)
        atomically (readTVar state)
    if play s
        -- Go from play to score
        then do
            now <- liftIO getCurrentTime
            liftIO $ atomically (writeTVar state (Scoreboard now))
            sendToAll =<< makeScoreMsg
        -- Go from score to play
        else do
            g <- liftIO $ atomically (takeTMVar next_grid)
            now <- liftIO getCurrentTime
            liftIO $ atomically $ do
                writeTVar state (Play g now)
                resetScores user_db
            (`withJust` sendToAll) =<< makeGridMsg

nextChange :: RusselM Integer
nextChange = do
    s <- liftIO . atomically . readTVar =<< asks state
    now <- liftIO getCurrentTime
    let running = diffToMs (diffUTCTime now (since s))
        delay = if play s then playLength else scoreLength
    return (delay - running)

gridMaker :: RusselM ()
gridMaker = do
    RusselEnv{trigrams,next_grid} <- ask
    forever $ liftIO $ do
        g <- convertOldGrid <$> makeOldGrid trigrams
        putStrLn "Grid created!"
        atomically (putTMVar next_grid g)
        putStrLn "Grid put in mvar!"

notify :: RusselM ()
notify = forever $ do
    liftIO $ threadDelay (1000 * 1000)
    sendToAll =<< makeScoreMsg

-- Make the grid message
makeGridMsg :: RusselM (Maybe ServerProtocol)
makeGridMsg = withGrid $ \ g -> do
    timeout <- (`div` 1000) <$> nextChange
    return (Grid timeout g (M.toList charScores))

-- Make the score board message
makeScoreMsg :: RusselM ServerProtocol
makeScoreMsg = do
    RusselEnv{state,user_db} <- ask
    (s,udb) <- liftIO $ atomically $
        liftM2 (,) (readTVar state) (readTVar user_db)
    timeout <- (`div` 1000) <$> nextChange
    case s of
        Play{} -> do
            let scores =
                    [ (u,user_score,user_words)
                    | (u,User{..}) <- M.toList udb
                    ]
            return (ScoreBoard timeout scores)
        Scoreboard{} -> do
            let scores =
                    [ (u,user_history)
                    | (u,User{..}) <- M.toList udb
                    ]
            return (FinalScores timeout scores)

loginHandler :: Sink -> Handler RusselM ClientProtocol
loginHandler sink = login
  where
    login = Handler $ \ msg -> case msg of
        Connect{..} -> do

            RusselEnv{user_db} <- ask
            liftIO $ do
                atomically (modifyTVar user_db (M.insert username (emptyUser sink)))
                send sink (Connected username)

            flip withJust (send sink) =<< makeGridMsg

            send sink =<< makeScoreMsg

            return (listen username)
        _ -> return login

    -- Handle snake submit messages
    listen name = always $ \ msg -> void $ withGrid $ \ grid -> case msg of
        Submit{..} -> (send sink =<<) $ do
            RusselEnv{lexicon} <- ask
            case snakeOnGrid lexicon grid charScores snake of
                Nothing -> return Response
                    { correct = False
                    , score = 0
                    }
                Just (word_text,value) -> do
                    RusselEnv{user_db} <- ask
                    placed_before <- liftIO $ atomically $ do
                        ok <- not <$> userPlaced name word_text user_db
                        let (mod_score,mod_history)
                                | ok        = ((+ value),((word_text,value):))
                                | otherwise = (id,id)
                        void (userMod sink name mod_score mod_history user_db)
                        return ok
                    return Response
                        { correct = placed_before
                        , score = if placed_before then value else 0
                        }
        _ -> return ()

handleError :: Sink -> SomeException -> RusselM ()
handleError sink _ = do
    RusselEnv{user_db} <- ask
    liftIO $ atomically $ modifyTVar user_db $ M.filter $ \ User{..} ->
        user_sink /= sink

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
    , ('W',10) -- non-standard score
    , ('X',10)
    , ('Y',8)
    , ('Z',10)
    , ('Å',4)
    , ('Ä',4)
    , ('Ö',4)
    ]

withJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
withJust (Just m) f = f m
withJust Nothing  _ = return ()

