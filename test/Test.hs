module Main (main) where

import Bobek.Test.MoverTest (moverSpec)
import Bobek.Test.ScriptFilterTest (scriptFilterSpec)
import Test.Hspec

main :: IO ()
main = hspec $ do
  moverSpec
  scriptFilterSpec
