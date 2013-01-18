{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Grid where

import Control.Applicative

import Data.Maybe
import Data.List
import Data.Ord

import Control.Monad
import Control.Monad.Random
import Control.Monad.Random.Class

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

type Coord = (Int,Int)
type Grid = [Text]

trigramCoordsFrom :: Coord -> [[Coord]]
trigramCoordsFrom x0 = nub $
    -- Around x0
    concat
    [ [ [x1,x0,x2]
      | x2 <- neighbours x0, okCoord x2, x2 /= x1
      ]
    | x1 <- neighbours x0, okCoord x1
    ] ++
    -- Starting/ending around x0
    concat
    [ concat
      [ [[x0,x1,x2],[x2,x1,x0]]
      | x2 <- neighbours x1, okCoord x2, x2 /= x0
      ]
    | x1 <- neighbours x0, okCoord x1
    ]

placeTrigram :: [Coord] -> Text -> Grid -> Grid
placeTrigram x t g = foldl (\g' (x',c) -> update g' x' c) g (zip x (T.unpack t))

type Trigrams = HashMap Text Int

lookupTrigram :: Trigrams -> Grid -> [Coord] -> Maybe Int
lookupTrigram t g xs = HM.lookup (wordAt g xs) t

wordAt :: Grid -> [Coord] -> Text
wordAt g = T.pack . map (g `at`)

trigramsAround' :: Trigrams -> Grid -> Coord -> [(Grid,Maybe Int)]
trigramsAround' t g x =
    [ (g',lookupTrigram t g' xs)
    | c <- ['A'..'Z'] ++ "ÅÄÖ"
    , let g' = update g x c
    , xs <- trigramCoordsFrom x
    ]

trigramsAround :: Trigrams -> Grid -> Coord -> [(Grid,Int)]
trigramsAround t g x = sortBy (flip $ comparing snd)
    [ (g,x) | (g,Just x) <- trigramsAround' t g x]

-- Fails if there is no trigrams around this coordinate,
pickSomeTrigram :: MonadRandom m => Trigrams -> Grid -> m (Maybe Grid)
pickSomeTrigram t g = do
    let xs = emptyCoords g
    x <- pickRandomElt xs
    case trigramsAround t g x of
        [] -> return Nothing
        grids -> do
            k <- getRandomR (2000,20000)
            u <- getRandomR (5,20)
            let grids' = filter ((> k) . snd) grids
                n = length grids' `div` u + 1
            case grids' of
                [] -> return Nothing
                _  -> (Just . fst) `liftM` pickRandomElt (take n grids')

makeGrid :: MonadRandom m => Trigrams -> m Grid
makeGrid t = do
    (t0,_) <- pickRandomElt (HM.toList t)
    x <- getRandomR (0,3)
    y <- getRandomR (0,3)
    xs <- pickRandomElt (trigramCoordsFrom (x,y))
    iter (placeTrigram xs t0 emptyGrid)
  where
    iter g
        | length (emptyCoords g) == 0 = return g
        | otherwise = do
            -- mapM_ T.putStrLn g
            -- T.putStrLn ""
            g' <- pickSomeTrigram t g
            case g' of
                Just g'' -> iter g''
                Nothing  -> iter g

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

pickRandomElt :: MonadRandom m => [a] -> m a
pickRandomElt xs = do
    let n = length xs
    i <- getRandomR (0,n-1)
    return (xs !! i)

coords :: [Coord]
coords = concat $ [ [ (x,y) | x <- [0..3] ] | y <- [0..3] ]

emptyCoords :: Grid -> [Coord]
emptyCoords g = filter (emptyPos g) coords


