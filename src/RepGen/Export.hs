--------------------------------------------------------------------------------
module RepGen.Export
  ( exportJSON
  , exportPgn
  ) where
--------------------------------------------------------------------------------
import RepGen.Monad
import RepGen.Config.Type
import RepGen.State.Type
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
import qualified Foreign.C.String as FC
import qualified RepGen.PyChess as PyC
--------------------------------------------------------------------------------

-- | Write the current MoveTree as a JSON file
exportJSON :: RGM ()
exportJSON = do
  tree <- use moveTree
  treePath <- view exportTreePath
  writeFile (unpack treePath) (toStrict $ J.encode tree)

  info <- use posToInfo
  infoPath <- view exportInfoPath
  writeFile (unpack infoPath) (toStrict $ J.encode info)

-- | Write the current MoveTree as a PGN file
exportPgn :: RGM ()
exportPgn = do
  tree <- use moveTree
  cTree <- liftIO . FC.newCString . unpack . decodeUtf8 . toStrict $ J.encode tree
  pModule <- use chessHelpers
  cResult <- liftIO $ PyC.tree_to_pgn pModule cTree
  pgn <- liftIO $ FC.peekCString cResult
  path <- view exportPgnPath
  writeFile (unpack path) (encodeUtf8 $ pack pgn)
