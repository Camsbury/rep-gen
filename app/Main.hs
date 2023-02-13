module Main where
import Prelude
import RepGen

main :: IO ()
main = do
  putStrLn =<< ucisToFen ["e2e4", "e7e5"]
  putStrLn =<< sansToUcis ["e4", "e5"]
  putStrLn =<< ucisToEngineCandidates ["e2e4", "e7e5"] "white" 20 10
