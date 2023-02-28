{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------------------
module RepGen.PyChess
  ( initChessHelpers
  , ucisToFen
  , sansToUcis
  , pyFinalize
  , fen_to_engine_candidates
  , tree_to_pgn
  ) where
--------------------------------------------------------------------------------
import Foreign.Ptr
import Foreign.C.String
import RepGen.Type
import RepGen.PyChess.Type
--------------------------------------------------------------------------------
import qualified Data.Text as T
--------------------------------------------------------------------------------

foreign import ccall "py_setup" py_setup :: IO ()
foreign import ccall "Py_Finalize" py_finalize :: IO ()
foreign import ccall "chess_helpers" chess_helpers :: IO (Ptr PyObject)
foreign import ccall "ucis_to_fen" ucis_to_fen
  :: Ptr PyObject -> CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis
  :: Ptr PyObject -> CString -> IO CString
foreign import ccall "fen_to_engine_candidates" fen_to_engine_candidates
  :: Ptr PyObject -> CString -> Int -> Int -> IO CString
foreign import ccall "tree_to_pgn" tree_to_pgn
  :: Ptr PyObject -> CString -> IO CString
--------------------------------------------------------------------------------

initChessHelpers :: IO (Ptr PyObject)
initChessHelpers = py_setup >> chess_helpers

pyFinalize :: IO ()
pyFinalize = py_finalize

-- | Convert a sequence of UCI 'Text' into a FEN 'Text'
ucisToFen
  :: (MonoFoldable m, Element m ~ Text)
  => Ptr PyObject
  -> m
  -> IO Fen
ucisToFen pModule ucis = do
  cUcis <- newCString . unpack $ intercalate "," ucis
  result <- ucis_to_fen pModule cUcis
  Fen . pack <$> peekCString result

-- | Convert a sequence of SAN 'Text' into UCI 'Text'
sansToUcis
  :: (MonoFoldable m, Element m ~ Text)
  => Ptr PyObject
  -> m
  -> IO (Vector Text)
sansToUcis pModule sans = do
  cSans <- newCString . unpack $ intercalate "," sans
  cResult <- sans_to_ucis pModule cSans
  result <- peekCString cResult
  pure . fromList . filter nonEmpty . T.splitOn "," . pack $ result
  where
    nonEmpty "" = False
    nonEmpty _  = True
