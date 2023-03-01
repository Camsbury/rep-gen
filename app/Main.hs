{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import RepGen

-- TODO: just take a JSON string for config so I can call from clojure
main :: IO ()
main = do
  buildRepertoire $
    def & colorL   .~ Black
        & mastersP .~ False
        & historyConfig . historyRatings .~ [L1800, L2000, L2200, L2500]
        & minProbAgg .~ 0.003
        & strategy . satisficers . engineFilter . engineAllowableLoss .~ 0.8
        & strategy . satisficers . engineFilter . engineDepth .~ 13
        -- & initRespProb .~ 0.1
        -- & asymRespProb .~ 0.25

        -- & startingMoves .~ ["Nf3"]
        -- & minLogLevel .~ LevelDebug


        & startingMoves .~
          [ "e4"
          , "Nc6"
          , "Nf3"
          ]

        -- & minLogLevel .~ LevelDebug
        -- & mOverrides .~ mapFromList ([([], "d4")] :: [([San], San)])
        -- & startingMoves .~ ["f4"]
