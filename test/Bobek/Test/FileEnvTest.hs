module Bobek.Test.FileEnvTest (fileEnvSpec) where

import Bobek.Destination (PublishResult (..))
import Bobek.FileEnv (serializeMessage, writeToFile)
import Bobek.Message (Message (..))
import Bobek.ReceiveId (ReceiveId (..))
import qualified Data.ByteString.Char8 as BSC (ByteString)
import Test.Hspec

msg :: Message
msg = MkMessage (MkReceiveId 42) "routing key" "message body"

happyCase :: [Message] -> IO ([BSC.ByteString], PublishResult)
happyCase msgs = do
  publishedMessagesRef <- newIORef ([] :: [BSC.ByteString])
  publishResult <- writeToFile (pure ()) (\line -> modifyIORef publishedMessagesRef (line :)) msgs
  publishedMessages <- readIORef publishedMessagesRef
  pure (publishedMessages, publishResult)

fileEnvSpec :: Spec
fileEnvSpec =
  describe "file env"
    $ describe "writefile"
    $ do
      it "should publish the serialized message" $ do
        (publishedMessages, _) <- happyCase $ replicate 100 msg
        publishedMessages `shouldBe` replicate 100 (serializeMessage msg)
      it "publish success should contain the receive id" $ do
        (_, publishResult) <- happyCase [msg]
        receiveId msg `elem` succeeded publishResult `shouldBe` True
