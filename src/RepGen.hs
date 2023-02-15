module RepGen
  ( module RepGen.PyChess
  , module RepGen.Type
  , module RepGen.Config.Type
  , module RepGen.State.Type
  , module RepGen.Monad
  , buildRepertoire
  ) where

import Prelude

import RepGen.Action
import RepGen.Action.Type
import RepGen.Config.Type
import RepGen.Monad
import RepGen.MoveTree.Type
import RepGen.State (initState)
import RepGen.State.Type
import RepGen.PyChess
import RepGen.Type
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runStateT)
import Control.Monad.Logger (runStdoutLoggingT, logInfoN)

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
    tree <- buildTree
    pPrintDarkBg tree

