module Lexicon where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS

import Control.Applicative

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Read as T

type Trigrams = HashMap Text Int

readLexicon :: FilePath -> IO (HashSet Text)
readLexicon file = HS.fromList . T.lines <$> T.readFile file

readTrigrams :: FilePath -> IO Trigrams
readTrigrams file = HM.fromList . map frequency . T.lines <$>
    T.readFile file
  where
    frequency :: Text -> (Text,Int)
    frequency l = (T.tail w,either error fst (T.decimal n))
      where (n,w) = T.break (== ' ') l

