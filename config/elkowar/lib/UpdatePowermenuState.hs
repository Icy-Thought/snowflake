{-# OPTIONS_GHC -Wall #-}

import Data.Conduit.Process (callProcess)
import EWWLib (update)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  callProcess "eww" . update $
    case args of
      ["false"] ->
        [ ("power", "true"),
          ("nixClass", "nixOpen")
        ]
      _ ->
        [ ("power", "false"),
          ("nixClass", "nixClosed")
        ]