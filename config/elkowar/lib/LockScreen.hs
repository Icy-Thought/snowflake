{-# OPTIONS_GHC -Wall #-}

import EWWLib (update)
import System.Process (callCommand, callProcess)

main :: IO ()
main = do
  callProcess "eww" $
    update
      [ ("power", "false"),
        ("nixClass", "nixClosed")
      ]
  callCommand "swaylock"