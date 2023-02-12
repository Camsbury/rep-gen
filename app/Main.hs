module Main where
import Prelude
import RepGen

main :: IO ()
main = putStrLn =<< ucisToFen ["e2e4", "e7e5"]
