{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
-- | A somewhat efficient implementation of a trie set of Chars.
module Data.Text.Trie.Set
    ( TrieSet
    , size
    , empty
    , isEmpty
    , insert
    , insertString
    , follow
    , fromList
    , fromStringList
    , member
    ) where

import Prelude hiding (lookup)
import Data.Foldable (foldl')
import Data.Map (Map)
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Data
import Data.Typeable
import Data.Text.Trie.Map (TrieMap)
import qualified Data.Text.Trie.Map as TM
import Test.QuickCheck

-- | A set of strings of `Char`s.
newtype TrieSet = TrieSet { trieMap :: TrieMap () }
  deriving (Eq,Ord,Show,Data,Typeable)

instance Arbitrary TrieSet where
    arbitrary = fmap fromStringList arbitrary

-- | An inefficient implementation of number of elements
size :: TrieSet -> Int
size = TM.size . trieMap

-- | The empty trie
empty :: TrieSet
empty = TrieSet TM.empty

-- | Is there a top element of this set?
top :: TrieSet -> Bool
top = isJust . TM.top . trieMap

-- | Is this an empty trie set?
isEmpty :: TrieSet -> Bool
isEmpty = TM.isEmpty . trieMap

-- | Inserts a text to a set
insert :: Text -> TrieSet -> TrieSet
insert s = TrieSet . TM.insert s () . trieMap

-- | Inserts a string to a set
insertString :: String -> TrieSet -> TrieSet
insertString s = TrieSet . TM.insertString s () . trieMap

-- | Follows a char one step in a set
follow :: Char -> TrieSet -> Maybe TrieSet
follow c = fmap TrieSet . TM.follow c . trieMap

-- | Makes a set from a list of `Text`s
fromList :: [Text] -> TrieSet
fromList = foldl' (flip insert) empty

-- | Makes a set from a list of `Strings`.
fromStringList :: [String] -> TrieSet
fromStringList = foldl' (flip insertString) empty

-- | Is this `Text` a member of the set?
member :: Text -> TrieSet -> Bool
member s = isJust . TM.lookup s . trieMap

-- | Is this string a member of the set?
memberString :: String -> TrieSet -> Bool
memberString s = isJust . TM.lookupString s . trieMap

