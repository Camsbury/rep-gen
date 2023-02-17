{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}

module RepGen.PyChess
  ( ucisToFen
  , sansToUcis
  , ucisToEngineCandidates
  ) where



import Foreign.C.String
import RepGen.PyChess.Internal ()
import RepGen.PyChess.Type

import qualified Data.Aeson as J
import qualified Data.Text as T

foreign import ccall "ucis_to_fen" ucis_to_fen
  :: CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis
  :: CString -> IO CString
foreign import ccall "ucis_to_engine_candidates" ucis_to_engine_candidates
  :: CString -> Int -> Int -> IO CString

--------------------------------------------------------------------------------
--- API
--------------------------------------------------------------------------------

-- | Convert a sequence of UCI 'Text' into a FEN 'Text'
ucisToFen :: (MonoFoldable m, Element m ~ Text) => m -> IO Text
ucisToFen ucis = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  result <- ucis_to_fen cUcis
  pack <$> peekCString result

-- TODO: Split the result into a Vector (or maybe the same form as the input)
-- | Convert a sequence of SAN 'Text' into UCI 'Text'
sansToUcis :: (MonoFoldable m, Element m ~ Text) => m -> IO (Vector Text)
sansToUcis sans = do
  cSans <- newCString . unpack $ intercalate "," sans
  cResult <- sans_to_ucis cSans
  result <- peekCString cResult
  pure . fromList . T.splitOn "," . pack $ result

-- | Convert a sequence of UCI 'Text' into a Vector/sequence of 'EngineCandidate'
ucisToEngineCandidates
  :: (MonoFoldable m, Element m ~ Text)
  => m
  -> Int
  -> Int
  -> IO (Either Text (Vector EngineCandidate))
ucisToEngineCandidates ucis depth mCount = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  cResult <- ucis_to_engine_candidates cUcis depth mCount
  result <- peekCString cResult
  pure . first pack . J.eitherDecode $ fromString result
