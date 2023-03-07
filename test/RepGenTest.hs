--------------------------------------------------------------------------------
module RepGenTest where
--------------------------------------------------------------------------------
-- import Test.Prelude
-- import Test.Support
--------------------------------------------------------------------------------
import RepGen
--------------------------------------------------------------------------------
import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range
--------------------------------------------------------------------------------

main :: IO ()
main
  = defaultMain
  . localOption (HedgehogTestLimit $ Just 5)
  $ testGroup "rep gen stuff"
    [ test_example
    ]
