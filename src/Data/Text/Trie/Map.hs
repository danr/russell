{-# LANGUAGE DeriveDataTypeable, DeriveFunctor #-}
-- | A somewhat efficient implementation of trie map of Chars.
module Data.Text.Trie.Map
    ( TrieMap
    , invariant
    , size
    , empty
    , isEmpty
    , top
    , insert
    , insertString
    , follow
    , fromList
    , fromStringList
    , lookup
    , lookupString
    ) where

import Prelude hiding (lookup,all)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe,isNothing,isJust)
import Data.Data
import Data.Foldable
import Data.Monoid
import Test.QuickCheck

-- Invariant: if the node is Nothing, but the Map isn't empty, it does
-- contain some values.

-- | A mapping from strings of `Char`s to @a@s, using `Data.Map.Map`.
data TrieMap a = Node !(Maybe a) !(Map Char (TrieMap a))
  deriving (Eq,Ord,Show,Data,Typeable,Functor)

instance Arbitrary a => Arbitrary (TrieMap a) where
    arbitrary = fmap fromStringList arbitrary

-- | The trie map invariant
invariant :: TrieMap a -> Bool
invariant (Node _ m) = all innerInvariant m

innerInvariant :: TrieMap a -> Bool
innerInvariant (Node e m) = all innerInvariant m && (isJust e || not (M.null m))

-- | Inefficient implementation of number of elements
size :: TrieMap a -> Int
size (Node x m) = M.fold (\ m' v -> size m' + v) (maybe 0 (const 1) x) m

-- | The empty trie
empty :: TrieMap a
empty = Node Nothing M.empty

-- | Is this map empty?
isEmpty :: TrieMap a -> Bool
isEmpty (Node x m) = isNothing x && M.null m

-- | The top element of a trie map
--
-- > top m = lookup "" m
top :: TrieMap a -> Maybe a
top (Node x _) = x

-- | Inserts a value into the trie
insert :: Text -> a -> TrieMap a -> TrieMap a
insert s v (Node x m)
    | T.null s  = Node (Just v) m
    | otherwise = Node x rec
  where
    rec = M.alter (Just . insert (T.tail s) v . fromMaybe empty) (T.head s) m

-- | Inserts a string into the trie
insertString :: String -> a -> TrieMap a -> TrieMap a
insertString []    v (Node _ m) = Node (Just v) m
insertString (c:s) v (Node x m) = Node x rec
  where
    rec = M.alter (Just . insertString s v . fromMaybe empty) c m

-- | Gets the subtrie from following this `Char` if there is one
follow :: Char -> TrieMap a -> Maybe (TrieMap a)
follow c (Node _ m) = M.lookup c m

-- | Makes a `TrieMap` from a list
fromList :: [(Text,a)] -> TrieMap a
fromList = foldl' (flip (uncurry insert)) empty

-- | Makes a `TrieMap` from a list
fromStringList :: [(String,a)] -> TrieMap a
fromStringList = foldl' (flip (uncurry insertString)) empty

-- | Looks up what a `Text` string is associated with, if anything
lookup :: Text -> TrieMap a -> Maybe a
lookup s (Node x m)
    | T.null s  = x
    | otherwise = lookup (T.tail s) =<< M.lookup (T.head s) m

-- | Looks up what a `String` string is associated with, if anything
lookupString :: String -> TrieMap a -> Maybe a
lookupString []    (Node x _) = x
lookupString (c:s) (Node _ m) = lookupString s =<< M.lookup c m

instance Foldable TrieMap where
    foldMap f (Node x m) = maybe mempty f x `mappend` foldMap (foldMap f) m

