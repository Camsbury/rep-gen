--------------------------------------------------------------------------------
module RepGen.Export
  ( exportJSON
  , exportPgn
  ) where
--------------------------------------------------------------------------------
import RepGen.Monad
import RepGen.Config.Type
import RepGen.State.Type
import RepGen.MoveTree.Type
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

-- | Write the current MoveTree as a JSON file
exportJSON :: RGM ()
exportJSON = do
  tree <- use moveTree
  path <- view exportJSONPath
  writeFile (unpack path) (toStrict $ J.encode tree)

-- | Write the current MoveTree as a PGN file
exportPgn :: RGM ()
exportPgn = undefined
