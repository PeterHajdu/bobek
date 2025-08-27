module Bobek.Test.RabbitMqEnvTest (rabbitMqEnvSpec) where

import Bobek.RabbitMqEnv (rabbitAcknowledge)
import Bobek.ReceiveId (ReceiveId (..))
import Control.Exception.Safe
import Network.AMQP (AMQPException (..))
import Test.Hspec

type MultipleFlag = Bool

happyCase :: [ReceiveId] -> IO [(ReceiveId, MultipleFlag)]
happyCase recIds = do
  ackCalls <- newIORef ([] :: [(ReceiveId, Bool)])
  rabbitAcknowledge (\recId multiple -> modifyIORef ackCalls ((MkReceiveId recId, multiple) :)) recIds
  reverse <$> readIORef ackCalls

someReceiveIds :: [ReceiveId]
someReceiveIds = [MkReceiveId 1, MkReceiveId 2]

anAmqpException :: AMQPException
anAmqpException = AllChannelsAllocatedException 0

rabbitMqEnvSpec :: Spec
rabbitMqEnvSpec =
  describe "rabbitmqenv" $
    describe "rabbitAcknowledge" $
      do
        it "calls ack with every receive id" $ do
          calls <- happyCase someReceiveIds
          (fst <$> calls) `shouldBe` someReceiveIds
        it "calls ack with multiple flag set to false" $ do
          calls <- happyCase someReceiveIds
          (snd <$> calls) `shouldBe` [False, False]
        it "handles amqp exceptions" $
          rabbitAcknowledge (\_ _ -> impureThrow anAmqpException) someReceiveIds
