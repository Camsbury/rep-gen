{-# LANGUAGE ForeignFunctionInterface #-}

module PyChess where

import Foreign.C.String
import Foreign.C.Types
import Data.List

foreign import ccall "ucis_to_fen" ucis_to_fen :: CString -> IO CString
foreign import ccall "sans_to_ucis" sans_to_ucis :: CString -> IO CString

ucisToFen :: [String] -> IO String
ucisToFen ucis = do
  cStr <- newCString $ intercalate "," ucis
  result <- ucis_to_fen cStr
  peekCString result

sansToUcis :: [String] -> IO String
sansToUcis sans = do
  cStr <- newCString $ intercalate "," sans
  result <- sans_to_ucis cStr
  peekCString result
