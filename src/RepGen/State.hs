--------------------------------------------------------------------------------
module RepGen.State where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.MoveTree.Type
import RepGen.State.Type
--------------------------------------------------------------------------------
import qualified RepGen.PyChess as PyC
--------------------------------------------------------------------------------

-- | Initialize the state of the repertoire generator
initState :: (MonadReader RGConfig m, MonadIO m) => m RGState
initState = do
  color <- view colorL
  node <- initTree
  pure . RGState node $ initActions color node

initTree :: (MonadReader RGConfig m, MonadIO m) => m TreeNode
initTree = do
  moves <- view startingMoves
  ucis <- liftIO $ PyC.sansToUcis moves
  foldl' addChild baseNode ucis

initActions :: Color -> TreeNode -> [RGAction]
initActions = undefined

baseNode
  :: (MonadReader RGConfig m, MonadIO m)
  => m TreeNode
baseNode = do
  undefined

addChild
  :: (MonadReader RGConfig m, MonadIO m)
  => m TreeNode
  -> Uci
  -> m TreeNode
addChild = do
  undefined

isMyTurn :: Color -> Vector Uci -> Bool
isMyTurn White ucis = even $ length ucis
isMyTurn Black ucis = odd  $ length ucis

leafNode :: TreeNode -> TreeNode
leafNode = undefined

--         stack
--         (if (my-turn? color ucis)
--           (list
--            {:action    :candidates
--             :ucis      ucis
--             :cons-prob 1.0
--             :depth     0}
--            {:action :prune
--             :ucis   ucis}
--            {:action :trans-stats
--             :ucis   ucis})
--           (list
--            {:action :init-responses
--             :ucis   ucis}
--            {:action :calc-stats
--             :ucis   ucis}))

-- (defn init-score
--   [{:keys [node] :as opts}]
--   (->> opts
--        ngn/prepare-engine-candidates
--        (filter #(= node (:uci %)))
--        first
--        :score))

-- (defn init-move-eval
--   [{:keys [color ucis uci prob-agg masters?] :as opts}]
--   (let [move-eval
--         (->> (assoc opts :group :lichess)
--              h/historic-moves
--              (filter #(= uci (:uci %)))
--              first)

--         masters-eval (when masters?
--                        (->> (assoc opts :group :masters)
--                             h/historic-moves
--                             (filter #(= uci (:uci %)))
--                             first))

--         move-eval (-> move-eval
--                       (assoc :white-m (:white masters-eval))
--                       (assoc :black-m (:black masters-eval))
--                       (assoc :prob-m  (:prob  masters-eval)))

--         prob-agg (cond-> prob-agg
--                    (not (my-turn? color ucis))
--                    (* (:prob move-eval)))]

--     (-> move-eval
--         (merge
--          {:ucis    (conj ucis uci)
--           :score    (init-score opts)
--           :prob-agg prob-agg}))))
