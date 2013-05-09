module Lexicon where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Control.Applicative

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Read as T

import Data.Text.Trie.Set as Trie

type Trigrams = HashMap Text Int
type Lexicon = TrieSet

lexiconFile :: FilePath
lexiconFile = "lexicon/saldom-stripped"

trigramsFile :: FilePath
trigramsFile = "lexicon/saldom-trigram-count"

readLexicon :: IO Lexicon
readLexicon = Trie.fromList . T.lines <$> T.readFile lexiconFile

readTrigrams :: IO Trigrams
readTrigrams = HM.fromList . map frequency . T.lines <$>
    T.readFile trigramsFile
  where
    frequency :: Text -> (Text,Int)
    frequency l = (T.tail w,either error fst (T.decimal n))
      where (n,w) = T.break (== ' ') l

