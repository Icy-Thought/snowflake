{-# OPTIONS_GHC -Wall #-}

import Data.Char (toUpper)
import HyprlandLib
  ( getFocusedMonID,
    getMonitors,
    getWorkspaces,
  )
import System.Environment (getArgs)
import System.Process (callCommand)

main :: IO ()
main = do
  args <- getArgs
  maybeWorkspaces <- getWorkspaces
  maybeMons <- getMonitors
  case maybeMons of
    Just mons -> case getFocusedMonID mons of
      Just monID ->
        if length args == 1 && length (head args) == 3
          then do
            callCommand $ case maybeWorkspaces of
              Just wss ->
                "hyprctl dispatch workspace "
                  ++ show (length wss + 1)
                  ++ " && hyprctl dispatch renameworkspace "
                  ++ show (length wss + 1)
                  ++ " "
                  ++ head args
                  ++ " && eww close monitor"
                  ++ show monID
                  ++ "WSSwitcher"
              Nothing -> "eww close monitor" ++ show monID ++ "WSSwitcher"
          else callCommand $ "eww close monitor" ++ show monID ++ "WSSwitcher"  