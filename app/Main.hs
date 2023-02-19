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

  -- fen <- ucisToFen (["e2e4", "e7e6"] :: Vector Text)
  -- cands <- do
  --   runStdoutLoggingT
  --   . runExceptT
  --   . (`runReaderT` def)
  --   . (`runStateT` def)
  --   $ fenToEngineCandidates fen 20 10
  -- print cands
  -- historicMoves <- do
  --   runStdoutLoggingT
  --   . runExceptT
  --   . (`runReaderT` def)
  --   . (`runStateT` def)
  --   . historicMoves
  --   $ LichessParams
  --   { _lichessRatings = [L2000, L2200, L2500]
  --   , _lichessSpeeds  = [Bullet, Blitz, Rapid]
  --   , _universals
  --     = UniversalParams
  --     { _moveCount = 10
  --     , _fen = fen
  --     }
  --   }
    -- $ UniversalParams
    -- { _moveCount = 10
    -- , _fen = fen
    -- }
  -- print historicMoves

  buildRepertoire $
    def & colorL       .~ Black
        & engineConfig . engineAllowableLoss .~ 0.05
