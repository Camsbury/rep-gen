{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
module RepGen.Config where
--------------------------------------------------------------------------------
import Foreign.Ptr
import RepGen.Type
import RepGen.Config.Type
import RepGen.PyChess.Type
--------------------------------------------------------------------------------
import qualified Data.List as L
import qualified RepGen.PyChess as PyC
--------------------------------------------------------------------------------

-- | Compile config from user input
compileConfig
  :: ( MonadError RGError m
    , MonadIO m
    )
  => RGConfig
  -> Ptr PyObject
  -> m RGConfig
compileConfig config chessHelpers = do
  overrides <- toFenORs $ config ^. mOverrides
  smORs <- toFenORs . toSanORs $ config ^. startingMoves
  pure $ config & overridesL .~ (overrides `union` smORs)
  where
    toFenOR (sans, san) = do
      rawUcis <- liftIO . PyC.sansToUcis chessHelpers $ snoc sans san
      (ucis, uci)
        <- throwMaybe "Bad ucis from sans during override creation"
        $ unsnoc rawUcis
      fen <- liftIO $ PyC.ucisToFen chessHelpers ucis
      pure (fen, uci)
    toFenORs
      = fmap mapFromList
      . traverse toFenOR
      . mapToList
    toSanORs moves
      = mapFromList
      $ L.inits moves ^.. folded . to unsnoc . _Just

