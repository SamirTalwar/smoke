module Test.Smoke.HelloSpec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Hspec

hello :: String -> String
hello name = "Hello, " ++ name ++ "!"

spec :: Spec
spec =
  parallel $
  describe "Hello" $
  it "says hello" $
  require $
  property $ do
    name <- forAll $ Gen.string (Range.linear 1 10) Gen.alpha
    let message = hello name
    take 7 message === "Hello, "
