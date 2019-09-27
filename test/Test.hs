{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Test.Hspec
import FakeEnvironment
import Message
import Source
import Destination

testMessages :: [Message]
testMessages = replicate 100 (MkMessage "test message")

testIds :: [ReceiveId]
testIds = MkReceiveId <$> [1..(length testMessages)]

testMessagesToReceive :: [Either SourceError (ReceiveId, Message)]
testMessagesToReceive = do
  (receiveId, msg) <- (zip testIds testMessages)
  return $ Right (receiveId, msg)

publishSuccess :: PublishResult
publishSuccess = MkPublishResult [] testIds

main :: IO ()
main = hspec $ do
  describe "moveMessages" $ do
    it "should send messages received from the source" $ do
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] publishSuccess)
      (published result) `shouldBe` testMessages

    it "should acknowledge published messages" $ do
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] publishSuccess)
      (acknowledgedMessages result) `shouldBe` testIds
