{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Grid where

import Data.List
import Data.Ord
import Data.Array

import Control.Arrow (second)
import Control.Monad
import Control.Monad.Random hiding (next)

import qualified Data.HashMap.Strict as HM

import Data.Map (Map)
import qualified Data.Map as M

import Data.Text (Text)
import qualified Data.Text as T

import Data.Text.Trie.Set as Trie

import Lexicon

type Coord = (Int,Int)
type Snake = [Coord]

submitSnake :: [[Int]] -> Snake
submitSnake = map (\[x,y] -> (x,y))

snakeOnGrid :: Lexicon -> Grid -> Map Char Int -> [[Int]] -> Maybe (Text,Int)
snakeOnGrid lexicon grid char_scores snake
    | word_ok   = Just (word_text,value)
    | otherwise = Nothing
  where
    word      = map (grid !) (submitSnake snake)
    value     = sum (map (char_scores M.!) word) + length word
    word_text = T.pack word
    word_ok   = word_text `member` lexicon

type Grid = Array (Int,Int) Char

count :: Grid -> Lexicon -> [[Coord]]
count grid l = concat
    [ go i [] l'
    | i <- indices grid
    , Just l' <- [follow (grid ! i) l]
    ]
  where
    go :: Coord -> [Coord] -> Lexicon -> [[Coord]]
    go i visited l' = oneword ++ nextwords
      where
        oneword  | top l'  = [reverse (i:visited)]
                 | otherwise = []
        nextwords = concat $
            [ go x (i:visited) next
            | x <- (neighbours grid i)
            , x `notElem` visited
            , Just next <- [follow (grid ! x) l']
            ]

countDistribution :: [[Coord]] -> [(Int,Int)]
countDistribution = map (\x -> (length x, head x))
                  . group . sort . map length

neighbours :: Grid -> Coord -> [Coord]
neighbours grid (x,y) = filter (inRange $ bounds grid) $ concat
    [
        [ (x + dx,y + dy)
        | dx <- [-1..1]
        , dx /= 0 || dy /= 0
        ]
    | dy <- [-1..1]
    ]

boolToInt :: Bool -> Int
boolToInt = fromEnum

type OldGrid = [Text]

convertOldGrid :: OldGrid -> Grid
convertOldGrid grid =
  array ((0,0),(3,3)) [((n,m),grid `at` (n,m)) | n <- [0..3], m <- [0..3]]

trigramCoordsFrom :: Coord -> [[Coord]]
trigramCoordsFrom x0 = nub $
    -- Around x0
    concat
    [ [ [x1,x0,x2]
      | x2 <- oldNeighbours x0, okCoord x2, x2 /= x1
      ]
    | x1 <- oldNeighbours x0, okCoord x1
    ] ++
    -- Starting/ending around x0
    concat
    [ concat
      [ [[x0,x1,x2],[x2,x1,x0]]
      | x2 <- oldNeighbours x1, okCoord x2, x2 /= x0
      ]
    | x1 <- oldNeighbours x0, okCoord x1
    ]

oldNeighbours :: Coord -> [Coord]
oldNeighbours (x,y) = filter okCoord $ concat
    [
        [ (x + dx,y + dy)
        | dx <- [-1..1]
        , dx /= 0 || dy /= 0
        ]
    | dy <- [-1..1]
    ]

okCoord :: Coord -> Bool
okCoord (x,y) = 0 <= x && x < 4 && 0 <= y && y < 4


placeTrigram :: [Coord] -> Text -> OldGrid -> OldGrid
placeTrigram x t g = foldl (\g' (x',c) -> update g' x' c) g (zip x (T.unpack t))


lookupTrigram :: Trigrams -> OldGrid -> [Coord] -> Maybe Int
lookupTrigram t g xs = HM.lookup (wordAt g xs) t

wordAt :: OldGrid -> [Coord] -> Text
wordAt g = T.pack . map (g `at`)

trigramsAround' :: Trigrams -> OldGrid -> Coord -> [(OldGrid,Maybe Int)]
trigramsAround' t g x =
    [ (g',lookupTrigram t g' xs)
    | c <- chars
    , let g' = update g x c
    , xs <- trigramCoordsFrom x
    ]

trigramsAround :: Trigrams -> OldGrid -> Coord -> [(OldGrid,Int)]
trigramsAround t g x = sortBy (flip $ comparing snd)
    [ (g',x') | (g',Just x') <- trigramsAround' t g x ]

-- Fails if there is no trigrams around this coordinate,
pickSomeTrigram :: MonadRandom m => Trigrams -> OldGrid -> m (Maybe OldGrid)
pickSomeTrigram t g = do
    let xs = emptyCoords g
    x <- pickRandomElt xs
    case trigramsAround t g x of
        [] -> return Nothing
        grids -> do
            k <- getRandomR (2000,20000)
            u <- getRandomR (5,20)
            let grids' = filter (\(g',v) -> v > k && not (bad g')) grids
                n = length grids' `div` u + 1
            case grids' of
                [] -> return Nothing
                _  -> (Just . fst) `liftM` pickRandomElt (take n grids')

bad :: OldGrid -> Bool
bad g = any (\c -> length (filter (c ==) l) > 3) chars
  where
    l = T.unpack (T.concat g)

chars :: [Char]
chars = ['A'..'Z'] ++ "ÅÄÖ"

makeOldGrid :: MonadRandom m => Trigrams -> m OldGrid
makeOldGrid t = do
    (t0,_) <- pickRandomElt (HM.toList t)
    x <- getRandomR (0,3)
    y <- getRandomR (0,3)
    xs <- pickRandomElt (trigramCoordsFrom (x,y))
    iter (placeTrigram xs t0 emptyOldGrid)
  where
    iter g
        | null (emptyCoords g) = return g
        | otherwise = do
            -- mapM_ T.putStrLn g
            -- T.putStrLn ""
            g' <- pickSomeTrigram t g
            case g' of
                Just g'' -> iter g''
                Nothing  -> iter g

at :: OldGrid -> Coord -> Char
g `at` (x,y) = T.index (g !! y) (toEnum x)

update :: OldGrid -> Coord -> Char -> OldGrid
update (r:rs) (x,0) c =
    let x' = toEnum x
    in  (T.take x' r `T.append` T.singleton c `T.append` T.drop (x' + 1) r) : rs
update (r:rs) (x,y) c = r:update rs (x,y-1) c
update []     _     _ = error "update out of bounds"

emptyOldGrid :: OldGrid
emptyOldGrid = replicate 4 "    "

emptyPos :: OldGrid -> Coord -> Bool
emptyPos g x = g `at` x == ' '

pickRandomElt :: MonadRandom m => [a] -> m a
pickRandomElt xs = do
    let n = length xs
    i <- getRandomR (0,n-1)
    return (xs !! i)

coords :: [Coord]
coords = concat [ [ (x,y) | x <- [0..3] ] | y <- [0..3] ]

emptyCoords :: OldGrid -> [Coord]
emptyCoords g = filter (emptyPos g) coords

testgrid :: Grid
testgrid = listArray ((0,0),(0,3)) "KATT"

testbig :: Grid
testbig = listArray ((0,0),(3,3)) $ concat
    ["MALA"
    ,"OIST"
    ,"RTÖR"
    ,"ZZZZ"
    ]

danBoard :: IO Grid
danBoard = do
    trigrams <- readTrigrams
    grid <- makeOldGrid trigrams
    return $ convertOldGrid grid

test :: Grid -> IO (Int,[[Coord]])
test grid = do
    l <- readLexicon
    let w = count grid l
    return (length w,w)

testio :: IO (Int,[(Int,Int)])
testio = fmap (second countDistribution) . test =<< danBoard

