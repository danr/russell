{-# LANGUAGE ScopedTypeVariables,TemplateHaskell #-}
module Main where

import Prelude hiding (lookup)
import Data.Text.Trie.Map
import Test.QuickCheck
import Test.QuickSpec
import Data.Maybe
import Data.Typeable
import Test.QuickCheck.All

triemaps :: forall a . (Ord a,Typeable a,Arbitrary a) => a -> [Sig]
triemaps a =
    [ prelude a
    , ["t1","t2","t3"]  `vars` (undefined :: TrieMap a)
    , ["s1","s2","s3"]  `vars` (undefined :: String)
    , ["l1","l2","l3"]  `vars` (undefined :: [String])
    , ["c1","c2","c3"]  `vars` (undefined :: Char)
    , ["m1","m2","m3"]  `vars` (undefined :: Maybe a)
    , "invariant"    `fun1` (invariant :: TrieMap a -> Bool)
    , "empty"        `fun0` (empty :: TrieMap a)
    , "isEmpty"      `fun1` (isEmpty :: TrieMap a -> Bool)
    , "top"          `fun1` (top :: TrieMap a -> Maybe a)
    , "insert"       `fun3` (insertString :: String -> a -> TrieMap a -> TrieMap a)
    , "follow"       `fun2` (follow :: Char -> TrieMap a -> Maybe (TrieMap a))
    , "lookup"       `fun2` (lookupString :: String -> TrieMap a -> Maybe a)
    , "isNothing"    `fun1` (isNothing :: Maybe a -> Bool)
    , "isJust"       `fun1` (isJust :: Maybe a -> Bool)
    , "fromMaybe"    `fun2` (fromMaybe :: a -> Maybe a -> a)
    , "fromList"     `fun1` (fromStringList :: [(String,a)] -> TrieMap a)
    , "elem"         `fun3` (curry elem :: String -> a -> [(String,a)] -> Bool)
    , "zip"          `fun2` (zip :: [String] -> [a] -> [(String,a)])
    ]

prop_insert_lookup s (v :: Int) m = lookupString s (insertString s v m) == Just v

prop_insert_commute s1 v1 s2 (v2 :: Int) m = s1 /= s2 ==>
    insertString s1 v1 (insertString s2 v2 m) ==
    insertString s2 v2 (insertString s1 v1 m)

prop_follow_lookup c s (v :: Int) m =
    (lookupString s =<< follow c m) == lookupString (c:s) m

main :: IO ()
main = do
    $(quickCheckAll)
    quickSpec (triemaps (undefined :: Int))

