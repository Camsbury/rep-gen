{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import RepGen

main :: IO ()
main = do
  -- fen <- ucisToFen (["e2e4", "e7e5"] :: Vector Text)
  -- print fen

  -- ucis <- sansToUcis (["e4", "e5"] :: Vector Text)
  -- print ucis

  -- cands <- ucisToEngineCandidates
  --   (["e2e4", "e7e5"] :: Vector Text)
  --   20
  --   10
  -- print cands

  let url = "https://httpbin.org/get"
  let qps = mapFromList [("foo", "bar")]
  response <- getRequest url qps
  print response
