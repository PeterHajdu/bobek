module Bobek.Test.ScriptFilterTest (scriptFilterSpec) where

import Bobek.Filter
import Bobek.ScriptFilter
import Test.Hspec

scriptFilterSpec :: Spec
scriptFilterSpec = describe "script filters" $ do
  it "should parse ack from response line" $ do
    shouldAck (parseAction "ack") `shouldBe` True
    shouldAck (parseAction " ack ") `shouldBe` True
    shouldCopy (parseAction "ack") `shouldBe` False
    shouldCopy (parseAction " ack ") `shouldBe` False
  it "should parse copy from response line" $ do
    shouldCopy (parseAction "copy") `shouldBe` True
    shouldCopy (parseAction " copy ") `shouldBe` True
    shouldAck (parseAction "copy") `shouldBe` False
    shouldAck (parseAction " copy ") `shouldBe` False
  it "should combine copy and ack" $ do
    let ackcopy = "ackcopy"
    shouldAck (parseAction ackcopy) `shouldBe` True
    shouldCopy (parseAction ackcopy) `shouldBe` True
