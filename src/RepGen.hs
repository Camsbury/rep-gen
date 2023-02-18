module RepGen
  ( module RepGen.PyChess
  , module RepGen.Type
  , module RepGen.Config.Type
  , module RepGen.Lichess.History
  , module RepGen.State.Type
  , module RepGen.Monad
  , buildRepertoire
  ) where

import RepGen.Action (runAction)
import RepGen.Config.Type
import RepGen.Lichess.History
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.State (initState)
import RepGen.State.Type
import RepGen.PyChess
import RepGen.Type
import qualified Database.Persist.Sqlite as DP
import qualified Web

-- | build the move tree for the repertoire
buildTree :: RGM TreeNode
buildTree = do
  action <- uses actionStack . preview $ ix 0
  actionStack %= fromMaybe empty . tailMay
  maybe (use moveTree) ((>> buildTree) . runAction) action

-- | Build a chess repertoire from a config
buildRepertoire :: RGConfig -> IO ()
buildRepertoire rgConfig
  = void
  . runStdoutLoggingT
  . runExceptT
  . (`runReaderT` rgConfig)
  . (`runStateT` initState)
  $ do
    dbPath <- view cachePath
    liftIO . DP.runSqlite dbPath $ DP.runMigration Web.migrateAll
    tree <- buildTree
    undefined -- TODO: Finish this function by exporting or whatever

