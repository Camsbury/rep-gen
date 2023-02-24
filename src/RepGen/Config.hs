{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
module RepGen.Config where
--------------------------------------------------------------------------------
import RepGen.Type
import RepGen.Config.Type
--------------------------------------------------------------------------------
import qualified RepGen.PyChess as PyC
--------------------------------------------------------------------------------

-- | Compile config from user input
compileConfig
  :: ( MonadError RGError m
    , MonadIO m
    )
  => RGConfig
  -> m RGConfig
compileConfig config = do
  overrides
    <- fmap mapFromList
    . traverse toFenORs
    . mapToList
    $ config ^. mOverrides
  pure $ config & overridesL .~ overrides
  where
    toFenORs (sans, san) = do
      rawUcis <- liftIO . PyC.sansToUcis $ snoc sans san
      (ucis, uci)
        <- throwMaybe "Bad ucis from sans during override creation"
        $ unsnoc rawUcis
      fen <- liftIO $ PyC.ucisToFen ucis
      pure (fen, uci)
