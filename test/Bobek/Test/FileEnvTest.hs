module Bobek.Test.FileEnvTest (fileEnvSpec) where

import Bobek.Destination (PublishResult (..))
import Bobek.FileEnv (serializeMessage, writeToFile)
import Bobek.Message (Message (..))
import Bobek.ReceiveId (ReceiveId (..))
import qualified Data.ByteString.Char8 as BSC (ByteString)
import System.IO.Error (ioError, userError)
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
      describe "happy case" $ do
        it "should publish the serialized message" $ do
          (publishedMessages, _) <- happyCase $ replicate 100 msg
          publishedMessages `shouldBe` replicate 100 (serializeMessage msg)
        it "publish success should contain the receive id" $ do
          (_, publishResult) <- happyCase [msg]
          receiveId msg `elem` succeeded publishResult `shouldBe` True
      describe "io failure" $ do
        it "should not crash if flush throws an io exception" $ do
          _ <- writeToFile (ioError (userError "an error")) (const $ pure ()) [msg]
          pure ()
        it "publish failure should contain the receive id" $ do
          publishResult <- writeToFile (pure ()) (const $ ioError (userError "an error")) [msg]
          receiveId msg `elem` failed publishResult `shouldBe` True
