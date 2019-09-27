{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Test.Hspec
import FakeEnvironment
import Message
import Source

main :: IO ()
main = hspec $ do
  describe "moveMessages" $ do
    it "should send messages received from the source" $ do
      let testMessage = MkMessage "a message"
      let result = runMoveMessages (MkEnv [Right (MkReceiveId 1, testMessage)] [] [] [Right ()])
      head (published result) `shouldBe` testMessage
