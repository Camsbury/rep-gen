{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RepGen


-- TODO: just take a JSON string for config so I can call from clojure
main :: IO ()
main = do
  buildRepertoire $
    def & colorL   .~ White
        & mastersP .~ False
        -- & searchDepth .~ 3
