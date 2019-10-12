module Main(main) where

import ReceiveId(ReceiveId(..))
import Test.Hspec
import FakeEnvironment
import Message
import Source
import Destination

testMessages :: [Message]
testMessages = (\rid -> (MkMessage (MkReceiveId rid) "test message")) <$> [1..100]


testIds :: [ReceiveId]
testIds = receiveId <$> testMessages

testMessagesToReceive :: [Either SourceError Message]
testMessagesToReceive = Right <$> testMessages

publishSuccess :: PublishResult
publishSuccess = MkPublishResult [] testIds

someSucceeds :: [ReceiveId] -> PublishResult
someSucceeds succeededIds = let failedIds = filter (flip elem $ succeededIds) testIds
                           in MkPublishResult failedIds succeededIds

main :: IO ()
main = hspec $ do
  describe "moveMessages" $ do
    it "should send messages received from the source" $ do
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] publishSuccess)
      (published result) `shouldBe` testMessages

    it "should acknowledge published messages" $ do
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] publishSuccess)
      (acknowledgedMessages result) `shouldBe` testIds

    it "should acknowledge messages only if publishing succeeds" $ do
      let succeededIds = [MkReceiveId 10, MkReceiveId 20]
      let result = runMoveMessages (MkEnv testMessagesToReceive [] [] (someSucceeds succeededIds))
      (acknowledgedMessages result) `shouldBe` succeededIds
