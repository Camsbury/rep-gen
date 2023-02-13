{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import RepGen

main :: IO ()
main = do
  fen <- ucisToFen (["e2e4", "e7e5"] :: Vector Text)
  putStrLn
    . tshow
    $ fen
  ucis <- sansToUcis (["e4", "e5"] :: Vector Text)
  putStrLn
    . tshow
    $ ucis
  cands <- ucisToEngineCandidates
    (["e2e4", "e7e5"] :: Vector Text)
    White
    20
    10
  putStrLn
    . tshow
    $ cands
