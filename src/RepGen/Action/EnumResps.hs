--------------------------------------------------------------------------------
module RepGen.Action.EnumResps
  ( module RepGen.Action.EnumResps
  ) where
--------------------------------------------------------------------------------

import RepGen.Monad
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Lichess.History
--------------------------------------------------------------------------------

-- data EnumData
--   = EnumData
--   { _edUcis     :: Vector Uci
--   , _edProb     :: Double
--   , _edDepth    :: Int
--   , _edIsPruned :: Bool
--   } deriving (Show, Eq)
-- makeLenses ''EnumData

runAction :: EnumData -> RGM ()
runAction action = do
  let ucis = action ^. edUcis
  maybeMM <- prepareMastersMoves ucis
  lichessM <- historicMoves =<< getLichessParams ucis
  undefined


        -- responses
        -- (->> (assoc opts :group :lichess)
        --      h/historic-moves
        --      (filter #(or
        --                (not pruned?)
        --                (< min-prob-agg (* prob-agg (:prob %)))))

        --      (filter #(< min-plays (:play-count %)))
        --      (map (fn [move]
        --             (merge move
        --                    {:ucis      (conj ucis (:uci move))
        --                     :cons-prob (* cons-prob (:prob move))
        --                     :prob-m    (:prob (get-candidate masters-responses move))
        --                     :prob-agg  (* prob-agg (:prob move))}))))
