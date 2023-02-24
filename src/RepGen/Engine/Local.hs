--------------------------------------------------------------------------------
module RepGen.Engine.Local where
--------------------------------------------------------------------------------
import RepGen.Config.Type
import RepGen.Engine.Type
import RepGen.Strategy.Type
import RepGen.Type
--------------------------------------------------------------------------------
import qualified Foreign.C.String as FC
import qualified RepGen.PyChess as PyC
import qualified Data.Aeson as J
--------------------------------------------------------------------------------

fenToLocalCandidates
  :: ( MonadReader RGConfig m
    , MonadError  RGError  m
    , MonadIO m
    )
  => Fen
  -> m [EngineCandidate]
fenToLocalCandidates (Fen fen) = do
  depth
    <- view
    $ strategy
    . satisficers
    . engineFilter
    . engineDepth
  mCount
    <- view
    $ strategy
    . satisficers
    . engineFilter
    . engineMoveCount
  cFen <- liftIO . FC.newCString . unpack $ fen
  cResult <- liftIO $ PyC.fen_to_engine_candidates cFen depth mCount
  jsonString <- liftIO $ FC.peekCString cResult
  throwEither
    . first pack
    . J.eitherDecode
    $ fromString jsonString

