--------------------------------------------------------------------------------
module RepGen
  ( module RepGen.PyChess
  , module RepGen.Type
  , module RepGen.Config.Type
  , module RepGen.Lichess.History
  , module RepGen.State.Type
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
  . runStdoutLoggingT
  . runExceptT
  . (`runReaderT` rgConfig)
  $ do
    dbPath <- view cachePath
    liftIO . DP.runSqlite dbPath $ DP.runMigration Web.migrateAll
    s <- initState
    (`runStateT` s) $ do
      buildTree
      X.exportPgn

buildTree :: RGM ()
buildTree = do
  action <- uses actionStack . preview $ ix 0
  actionStack %= fromMaybe empty . tailMay
  maybe (pure ()) ((>> buildTree) . runAction) action

