{-# LANGUAGE OverloadedStrings #-}
module Grid where

import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T

type Coord = (Int,Int)
type Grid = [Text]

at :: Grid -> Coord -> Char
g `at` (x,y) = T.index (g !! y) (toEnum x)

update :: Grid -> Coord -> Char -> Grid
update (r:rs) (x,0) c =
    let x' = toEnum x
    in  (T.take x' r `T.append` T.singleton c `T.append` T.drop (x' + 1) r) : rs
update (r:rs) (x,y) c = r:update rs (x,y-1) c
update []     _     _ = error "update out of bounds"

emptyGrid :: Grid
emptyGrid = replicate 4 "    "

emptyPos :: Grid -> Coord -> Bool
emptyPos g x = g `at` x == ' '

neighbour :: Coord -> Coord -> Bool
neighbour (x,y) (x',y') = max (abs (x - x')) (abs (x + x')) == 1

neighbours :: Coord -> [Coord]
neighbours (x,y) = filter okCoord $ concat
    [
        [ (x + dx,y + dy)
        | dx <- [-1..1]
        , dx /= 0 || dy /= 0
        ]
    | dy <- [-1..1]
    ]

okCoord :: Coord -> Bool
okCoord (x,y) = 0 <= x && x < 4 && 0 <= y && y < 4

makeGrid :: MonadRandom m => [Text] -> m Grid
makeGrid words = fillRandomly =<< go 0 emptyGrid
  where
    go fails g
        | fails > 100 = return g
        | length (emptyCoords g) <= 2 = return g
        | otherwise = do
            w <- pickRandomElt words
            mg' <- tryPlace w g
            case mg' of
                Nothing -> go (fails + 1) g
                Just g' -> go 0 g'

fillRandomly :: MonadRandom m => Grid -> m Grid
fillRandomly g0 = fill g0 (emptyCoords g0)
  where
    fill g (x:xs) = do
        c <- pickRandomElt candidates
        fill (update g x c) xs
    fill g [] = return g
    candidates = "AAAADEEEEFGHIIIKLMNOOOPRSTUVYÅÄÖ"

pickRandomElt :: MonadRandom m => [a] -> m a
pickRandomElt xs = do
    let n = length xs
    i <- getRandomR (0,n-1)
    return (xs !! i)

tryPlace :: MonadRandom m => Text -> Grid -> m (Maybe Grid)
tryPlace w g = do
    let empty = emptyCoords g
    case empty of
        [] -> return Nothing
        xs -> tryPlace' w g =<< pickRandomElt xs

tryPlace' :: MonadRandom m => Text -> Grid -> Coord -> m (Maybe Grid)
tryPlace' w g x = case T.uncons w of
    Nothing -> return (Just g)
    Just (c,cs) -> do
        let g' = update g x c
            xs = filter (\ x' -> (g `at` x') `elem` [' ',c])
                        (neighbours x)
        case xs of
            [] -> return Nothing
            _  -> do
                x' <- pickRandomElt xs
                tryPlace' cs g' x'


coords :: [Coord]
coords = concat $ [ [ (x,y) | x <- [0..3] ] | y <- [0..3] ]

emptyCoords :: Grid -> [Coord]
emptyCoords g = filter (emptyPos g) coords


