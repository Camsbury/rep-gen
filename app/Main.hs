{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RepGen


main :: IO ()
main = do
  -- fen <- ucisToFen (["e2e4", "e7e5"] :: Vector Text)
  -- -- let fen = Fen "r1bqkb1r/pppp1ppp/2n2n2/4p2Q/2B1P3/8/PPPP1PPP/RNB1K1NR w KQkq - 0 1"
  -- cands
  --   <- runStdoutLoggingT
  --   . runExceptT
  --   . (`runReaderT` (def & engineConfig . engineAllowableLoss .~ 0.1))
  --   . (`runStateT` def)
  --   $ do
  --     fenToEngineCandidates fen
  -- print cands

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
    def & colorL      .~ White -- then verify black!!
        & searchDepth .~ 3
        & minProbAgg  .~ 0.1
