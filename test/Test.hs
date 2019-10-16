module Main(main) where

import ReceiveId(ReceiveId(..))
import Test.Hspec
import FakeEnvironment
import Message
import Source
import Destination

testMessages :: [Message]
testMessages = (\rid -> (MkMessage (MkReceiveId rid) "routing key" "test message")) <$> [1..100]


testIds :: [ReceiveId]
testIds = receiveId <$> testMessages

testMessagesToReceive :: [Either NoMessageReason Message]
testMessagesToReceive = Right <$> testMessages

publishSuccesses :: [PublishResult]
publishSuccesses = (MkPublishResult []) <$> ((:[]) <$> testIds)

someSucceeds :: [ReceiveId] -> [PublishResult]
someSucceeds succeededIds = (\rid -> if elem rid succeededIds
                                     then MkPublishResult [] [rid]
                                     else MkPublishResult [rid] []) <$> testIds

main :: IO ()
main = hspec $ do
  describe "moveMessages" $ do
    it "should send message received from the source" $ do
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] publishSuccesses)
      (published result) `shouldBe` (:[]) <$> testMessages

    it "should acknowledge published messages" $ do
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] publishSuccesses)
      (acknowledgedMessages result) `shouldBe` (:[]) <$> testIds

    it "should acknowledge messages only if publishing succeeds" $ do
      let succeededIds = [MkReceiveId 10, MkReceiveId 20]
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] (someSucceeds succeededIds))
      (acknowledgedMessages result) `shouldBe` ((:[]) <$> succeededIds)
