{-# LANGUAGE ForeignFunctionInterface #-}

module PyChess where

import Foreign.C.String
import Foreign.C.Types
import Data.List

foreign import ccall "ucis_to_fen" ucis_to_fen :: CString -> IO CString

ucisToFen :: [String] -> IO String
ucisToFen ucis = do
  cStr <- newCString $ intercalate "," ucis
  result <- ucis_to_fen cStr
  peekCString result
