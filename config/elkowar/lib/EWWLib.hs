{-# OPTIONS_GHC -Wall #-}

module EWWLib where

import System.Process (readCreateProcess, shell)

getVariableValue :: String -> IO String
getVariableValue var = filter (/= '\n') <$> readCreateProcess (shell $ "eww get " ++ var) ""

update :: [(String, String)] -> [String]
update = ("update" :) . map (\(var, val) -> var ++ "=" ++ val)

open :: [String] -> [String]
open = ("open-many" :)