{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
module RepGen.PyChess
  ( ucisToFen
  , sansToUcis
  , fenToEngineCandidates
  ) where
--------------------------------------------------------------------------------
import Foreign.C.String
import RepGen.Engine ()
import RepGen.Engine.Type
import RepGen.Monad
import RepGen.Type
--------------------------------------------------------------------------------
import qualified Data.Aeson as J
import qualified Data.Text as T
--------------------------------------------------------------------------------
foreign import ccall "ucis_to_fen" ucis_to_fen
  :: CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis
  :: CString -> IO CString
foreign import ccall "fen_to_engine_candidates" fen_to_engine_candidates
  :: CString -> Int -> Int -> IO CString
--------------------------------------------------------------------------------

-- | Convert a sequence of UCI 'Text' into a FEN 'Text'
ucisToFen :: (MonoFoldable m, Element m ~ Text) => m -> IO Fen
ucisToFen ucis = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  result <- ucis_to_fen cUcis
  Fen . pack <$> peekCString result

-- | Convert a sequence of SAN 'Text' into UCI 'Text'
sansToUcis :: (MonoFoldable m, Element m ~ Text) => m -> IO (Vector Text)
sansToUcis sans = do
  cSans <- newCString . unpack $ intercalate "," sans
  cResult <- sans_to_ucis cSans
  result <- peekCString cResult
  pure . fromList . T.splitOn "," . pack $ result

-- | Convert a 'Fen' into a Vector/sequence of 'EngineCandidate'
fenToEngineCandidates
  :: Fen
  -> Int
  -> Int
  -> RGM (Vector EngineCandidate)
fenToEngineCandidates (Fen fen) depth mCount = do
  cUcis <- liftIO . newCString . unpack $ fen
  cResult <- liftIO $ fen_to_engine_candidates cUcis depth mCount
  result <- liftIO $ peekCString cResult
  throwEither . first pack . J.eitherDecode $ fromString result
