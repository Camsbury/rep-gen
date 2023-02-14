{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude
import RepGen
import RepGen.Lichess.History

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

  fen <- ucisToFen (["e2e4", "e7e5"] :: Vector Text)
  -- let url = "https://explorer.lichess.ovh/masters"
  -- let qps = mapFromList [("fen", fen), ("moves", "10"), ("topGames", "0")]
  -- response <- getRequest url qps
  -- print response
  historicMoves
    <- historicMovesMasters
    $ UniversalParams
    { _moveCount = 10
    , _fen = fen
    }
  print historicMoves
