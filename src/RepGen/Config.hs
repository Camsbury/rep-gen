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
  exclusions <- toFenEXs $ config ^. mExclusions
  overrides <- toFenORs $ config ^. mOverrides
  smORs <- toFenORs . toSanORs $ config ^. startingMoves
  pure
    $ config
    & exclusionsL .~ exclusions
    & overridesL .~ (overrides `union` smORs)
  where
    toFenEXs exs = mconcat <$> traverse (toFenEX chessHelpers) exs
    toFenORs
      = fmap mapFromList
      . traverse (toFenOR chessHelpers)
    toSanORs moves
      = L.inits moves ^.. folded . to unsnoc . _Just


toFenOR
  :: ( MonadError RGError m
    , MonadIO m
    )
  => Ptr PyObject
  -> ([San], San)
  -> m (Fen, Uci)
toFenOR chessHelpers (sans, san) = do
  rawUcis <- liftIO . PyC.sansToUcis chessHelpers $ snoc sans san
  (ucis, uci)
    <- throwMaybe "Bad ucis from sans during override creation"
    $ unsnoc rawUcis
  fen <- liftIO $ PyC.ucisToFen chessHelpers ucis
  pure (fen, uci)


toFenEX
  :: ( MonadError RGError m
    , MonadIO m
    )
  => Ptr PyObject
  -> ([San], [San])
  -> m (Map Fen (Set Uci))
toFenEX chessHelpers (path, opts) = do
  let prefixedOpts = (path, ) <$> opts
  kvs :: [(Fen, Uci)] <- traverse (toFenOR chessHelpers) prefixedOpts
  pure $ foldr insertPair mempty kvs


insertPair :: (Ord a, Ord b) => (a, b) -> Map a (Set b) -> Map a (Set b)
insertPair (a, b) = insertWith union a (singleton b)
