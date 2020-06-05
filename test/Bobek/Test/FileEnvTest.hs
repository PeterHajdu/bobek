module Bobek.Test.FileEnvTest (fileEnvSpec) where

import Bobek.Destination
import Bobek.FileEnv (fileAsDestination, fileAsSource, readFromFile, serializeMessage, writeToFile)
import Bobek.Message (Message (..))
import Bobek.ReceiveId (ReceiveId (..))
import Bobek.Source
import qualified Data.ByteString.Char8 as BSC (ByteString)
import GHC.IO.Exception (IOErrorType (..), IOException (..))
import Polysemy
import qualified Polysemy.Reader as P
import Polysemy.Error (runError)
import System.IO.Error (ioError, userError)
import Test.Hspec

msg :: Message
msg = MkMessage (MkReceiveId 42) "routingkey" "message body"

happyCase :: [Message] -> IO ([BSC.ByteString], PublishResult)
happyCase msgs = do
  publishedMessagesRef <- newIORef ([] :: [BSC.ByteString])
  publishResult <- writeToFile (pure ()) (\line -> modifyIORef publishedMessagesRef (line :)) msgs
  publishedMessages <- readIORef publishedMessagesRef
  pure (publishedMessages, publishResult)

fileEnvSpec :: Spec
fileEnvSpec =
  describe "file based operations and interpreters" $ do
    describe "IO file operations" $ do
      describe "writefile" $ do
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

      describe "readfile" $ do
        it "parses messages with receiveId 0" $ do
          maybeMessage <- readFromFile (pure $ serializeMessage msg)
          maybeMessage `shouldBe` Right (msg {receiveId = MkReceiveId 0})
        it "handles io errors" $ do
          maybeMessage <- readFromFile (ioError $ userError "an error")
          maybeMessage `shouldBe` Left (NMRError "user error (an error)")
        it "handles missing routing key" $ do
          maybeMessage <- readFromFile (pure "somethingsomething")
          maybeMessage `shouldBe` Left (NMRError "Missing routing key.")
        it "handles end of file as an empty queue" $ do
          maybeMessage <- readFromFile (ioError $ IOError Nothing EOF "" "" Nothing Nothing)
          maybeMessage `shouldBe` Left NMREmptyQueue

    describe "interpreters" $ do
      describe "fileAsDestination" $ do
        it "should handle io errors during file open" $ do
          result <-
            publish []
              & P.runReader "hopefullynonexistentfile" . fileAsDestination
              & runError @Text
              & runM

    --       result `shouldBe` Left ("hopefullynonexistentfile: openFile: does not exist (No such file or directory)")

      -- describe "fileAsSource" $ do
      --   it "should handle io errors during file open" $ do
      --     result <-
      --       receive
      --         & P.runReader "hopefullynonexistentfile" . fileAsSource
      --         & runError @Text
      --         & runM

      --     result `shouldBe` Left ("hopefullynonexistentfile: openFile: does not exist (No such file or directory)")
