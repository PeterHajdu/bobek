module Main(main) where

import Test.Hspec
import FakeEnvironment
import Message
import Source

main :: IO ()
main = hspec $ do
  describe "moveMessages" $ do
    it "should send messages received from the source" $ do
      let result = runMoveMessages (MkEnv [Right (MkReceiveId 1, MkMessage)] [] [Right ()])
      length (published result) `shouldBe` 1
