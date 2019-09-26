module Mover(moveMessages) where

import Destination
import Source

moveMessages :: (Source m, Destination m) => m ()
moveMessages = return ()
