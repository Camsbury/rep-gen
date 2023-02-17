{-# LANGUAGE ScopedTypeVariables #-}
module Main where


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

  -- fen <- ucisToFen (["e2e4", "e7e5"] :: Vector Text)
  -- historicMoves
  --   <- historicMovesMasters
  --   $ UniversalParams
  --   { _moveCount = 10
  --   , _fen = fen
  --   }
  -- print historicMoves

  buildRepertoire $
    def & color        .~ Black
        & engineConfig . engineAllowableLoss .~ 0.05
