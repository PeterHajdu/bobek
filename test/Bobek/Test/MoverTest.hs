module Bobek.Test.MoverTest (moverSpec) where

import Bobek.Destination
import Bobek.Filter
import Bobek.Message
import Bobek.ReceiveId (ReceiveId (..))
import Bobek.Source ()
import Bobek.Test.FakeEnvironment
import Test.Hspec

makeId :: Integral a => a -> ReceiveId
makeId n = MkReceiveId $ fromIntegral n

makeMessages :: Int -> [Message]
makeMessages n = (\rid -> MkMessage (makeId rid) "routing key" "test message") <$> [1 .. n]

bothFilter :: Message -> FilterActions
bothFilter = const $ MkFilterActions [Copy, Ack]

twoBulkMessages :: [Message]
twoBulkMessages = makeMessages 1500

twoBulks :: [[Message]]
twoBulks = biList $ splitAt 1000 twoBulkMessages

twoBulksOfIds :: [[ReceiveId]]
twoBulksOfIds = fmap receiveId <$> twoBulks

allSucceeds :: [Message] -> PublishResult
allSucceeds msgs = MkPublishResult [] (receiveId <$> msgs)

someSucceeds :: [Message] -> PublishResult
someSucceeds msgs =
  let middle = (length msgs `div` 2)
      (f, s) = splitAt middle (receiveId <$> msgs)
   in MkPublishResult s f

onePageMessages :: [Message]
onePageMessages = makeMessages 1000

moverSpec :: Spec
moverSpec = describe "moveMessages" $ do
  it "should publish messages in one bulk if the number of messages is less than the bulk size" $ do
    let result = runMoveMessages (MkEnv (Right <$> onePageMessages) [] [] allSucceeds bothFilter)
    published result `shouldBe` [onePageMessages]
  it "should publish messages in more bulks if the number of messages is greater than the bulk size" $ do
    let result = runMoveMessages (MkEnv (Right <$> twoBulkMessages) [] [] allSucceeds bothFilter)
    published result `shouldBe` twoBulks
  it "should acknowledge published messages" $ do
    let result = runMoveMessages (MkEnv (Right <$> twoBulkMessages) [] [] allSucceeds bothFilter)
    acknowledgedMessages result `shouldBe` twoBulksOfIds
  it "should acknowledge messages only if publishing succeeds" $ do
    let toBeAcked = succeeded $ someSucceeds onePageMessages
    let result = runMoveMessages (MkEnv (Right <$> onePageMessages) [] [] someSucceeds bothFilter)
    acknowledgedMessages result `shouldBe` [toBeAcked]
  it "should acknowledge messages only if the filter asks for it" $ do
    let onlyCopy = const $ MkFilterActions [Copy]
    let result = runMoveMessages (MkEnv (Right <$> onePageMessages) [] [] allSucceeds onlyCopy)
    acknowledgedMessages result `shouldBe` []
  it "should copy the messages only if the filter asks for it" $ do
    let onlyAck = const $ MkFilterActions [Ack]
    let result = runMoveMessages (MkEnv (Right <$> twoBulkMessages) [] [] allSucceeds onlyAck)
    acknowledgedMessages result `shouldBe` twoBulksOfIds
    null (published result) `shouldBe` True
