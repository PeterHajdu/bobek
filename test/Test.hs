module Main (main) where

import Bobek.Test.FileEnvTest (fileEnvSpec)
import Bobek.Test.MoverTest (moverSpec)
import Bobek.Test.RabbitMqEnvTest (rabbitMqEnvSpec)
import Bobek.Test.ScriptFilterTest (scriptFilterSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  moverSpec
  scriptFilterSpec
  fileEnvSpec
  rabbitMqEnvSpec
