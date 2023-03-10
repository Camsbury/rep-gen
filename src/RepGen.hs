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
import RepGen.Config
import RepGen.Config.Type
import RepGen.Lichess.History
import RepGen.Monad
import RepGen.State (initState)
import RepGen.State.Type
import RepGen.Strategy.Type
import RepGen.PyChess
import RepGen.Type

--------------------------------------------------------------------------------
import qualified RepGen.Engine.Local as Ngn
import qualified RepGen.Export as X
import qualified Database.Persist.Sqlite as DP
import qualified Web
--------------------------------------------------------------------------------

-- | Build a chess repertoire from a config
buildRepertoire :: Either Text RGConfig -> IO ()
buildRepertoire rgConfigE
  = void
  . either print pure
  <=< runExceptT $ do
    pModule <- liftIO initChessHelpers
    rgConfig <- throwEither rgConfigE
    compiled <- compileConfig rgConfig pModule
    (`runReaderT` compiled) . runStdoutLoggingT $ do
      mLvl <- view minLogLevel
      filterLogger (lFilter mLvl) $ do
        ecPath <- view engineCachePath
        liftIO . DP.runSqlite ecPath $ DP.runMigration Ngn.migrateAll
        hcPath <- view httpCachePath
        liftIO . DP.runSqlite hcPath $ DP.runMigration Web.migrateAll
        iState <- initState pModule
        (`evalStateT` iState) $ do
          buildTree
          X.exportJSON
          X.exportPgn
          liftIO pyFinalize
  where
    lFilter mLvl _ lvl = lvl >= mLvl

buildTree :: RGM ()
buildTree = do
  aStack <- use actionStack
  logDebugN $ "Action stack: " <> tshow aStack
  action <- uses actionStack . preview $ ix 0
  actionStack %= fromMaybe empty . tailMay
  maybe (pure ()) ((>> buildTree) . runAction) action

