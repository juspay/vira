module System.TailSpec where

import System.Tail
import Test.Hspec

spec :: Spec
spec = describe "System.Tail" $ do
  it "says hello" $ do
    hello `shouldBe` "Hello from System.Tail!"
