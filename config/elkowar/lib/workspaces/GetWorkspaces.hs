{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wall #-}

import Data.Char (toUpper)
import Data.List (isPrefixOf, sort)
import Data.List.Split (chunksOf)
import Safe (headMay)
import System.Environment (getArgs)
import System.Process (callCommand)
import Text.RawString.QQ (r)

main :: IO ()
main = do
  args <- getArgs
  maybeWSs <- getWorkspaces
  case maybeWSs of
    Just wss ->
      let groupedWSs =
            chunksOf 3 $
              maybe id (filter . isPrefixOf . map toUpper) (headMay args) $
                sort wss
          selectedWSs = makeWSs groupedWSs
          command = unlines ["eww update selectedWSs=\"", selectedWSs, "\""]
       in callCommand command
    Nothing -> pure ()

makeWSs :: [[Workspace]] -> String
makeWSs wss =
  [r|(scroll 
  :height 150
  (box 
    :orientation "vertical"|]
    ++ concatMap makeWSRow wssPadded
    ++ [r|
  )
)|]
  where
    wssPadded = map padWSs wss
    padWSs ws = ws ++ replicate (3 - length ws) (Workspace "NIL")

    makeWSRow :: [Workspace] -> String
    makeWSRow = intercalate "\n" . map makeWS

    makeWS :: Workspace -> String
    makeWS ws = "(label :class \"switcherWS\" :text \"" ++ wsName ws ++ "\")"
