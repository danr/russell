{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Data.Monoid (mconcat)

import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Static
import Web.Scotty

main :: IO ()
main = scotty 3000 $ do

    middleware logStdout
    middleware $ staticPolicy $ mconcat
        [ addBase "frontend"
        , noDots
        , foldr1 (<|>) (map hasSuffix
            ["html", "js", "css", "jpg", "txt"])
        ] <|> only (zip ["","/"] (repeat "frontend/index.html"))

