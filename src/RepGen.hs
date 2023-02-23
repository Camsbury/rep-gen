--------------------------------------------------------------------------------
module RepGen
  ( module RepGen.PyChess
  , module RepGen.Type
  , module RepGen.Config.Type
  , module RepGen.Lichess.History
  , module RepGen.State.Type
  , module RepGen.Strategy.Type
  , module RepGen.Monad
  , buildRepertoire
  ) where
--------------------------------------------------------------------------------
import RepGen.Action (runAction)
import RepGen.Config.Type
import RepGen.Lichess.History
import RepGen.Monad
import RepGen.State (initState)
import RepGen.State.Type
import RepGen.Strategy.Type
import RepGen.PyChess
import RepGen.Type

--------------------------------------------------------------------------------
import qualified RepGen.Export as X
import qualified Database.Persist.Sqlite as DP
import qualified Web
--------------------------------------------------------------------------------

-- | Build a chess repertoire from a config
buildRepertoire :: RGConfig -> IO ()
buildRepertoire rgConfig
  = void
  . either print pure
  <=< runStdoutLoggingT
  . filterLogger lFilter
  . runExceptT
  . (`runReaderT` rgConfig)
  $ do
    dbPath <- view cachePath
    liftIO . DP.runSqlite dbPath $ DP.runMigration Web.migrateAll
    evalStateT (buildTree >> X.exportPgn) =<< initState
  where
    lFilter _ lvl = lvl > LevelDebug

buildTree :: RGM ()
buildTree = do
  action <- uses actionStack . preview $ ix 0
  actionStack %= fromMaybe empty . tailMay
  -- aStack <- use actionStack
  -- logDebugN $ tshow aStack
  maybe (pure ()) ((>> buildTree) . runAction) action

