module Bobek.Util (tshow) where

import Data.Text

tshow :: Show a => a -> Text
tshow = pack . show
