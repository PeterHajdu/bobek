module Main where

import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "something" $ do
    it "should fail" $ do
      1 `shouldBe` 2
