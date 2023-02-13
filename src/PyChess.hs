{-# LANGUAGE ForeignFunctionInterface #-}

module PyChess where

import Prelude
import Foreign.C.String
import Foreign.C.Types

foreign import ccall "ucis_to_fen" ucis_to_fen :: CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis :: CString -> IO CString
foreign import ccall "ucis_to_engine_candidates" ucis_to_engine_candidates :: CString -> CString -> Int -> Int -> IO CString

ucisToFen :: [String] -> IO String
ucisToFen ucis = do
  cUcis <- newCString $ intercalate "," ucis
  result <- ucis_to_fen cUcis
  peekCString result

sansToUcis :: [String] -> IO String
sansToUcis sans = do
  cSans <- newCString $ intercalate "," sans
  result <- sans_to_ucis cSans
  peekCString result

ucisToEngineCandidates :: [String] -> String -> Int -> Int -> IO String
ucisToEngineCandidates ucis color depth moveCount = do
  cUcis <- newCString $ intercalate "," ucis
  cColor <- newCString color
  result <- ucis_to_engine_candidates cUcis cColor depth moveCount
  peekCString result

