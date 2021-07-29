{- |
   Module : Theme.Xresources
   Copyright : (c) 2021 Joan Milev <joantmilev@gmail.com>
   License : MIT

   Maintainer : Joan Milev <joantmilev@gmail.com>
   Stability : Stable
   Portability : Unknown
-}

module Theme.Xresources (xprop) where

import           Data.Bifunctor   (bimap)
import           Data.Char        (isSpace)
import           Data.List        (dropWhileEnd, elemIndex, find)
import           Data.Maybe       (catMaybes, fromMaybe)
import           Prelude          (IO, Int, Maybe, ShowS, String, dropWhile,
                                   fst, lines, snd, splitAt, tail, ($), (.),
                                   (<$>), (==))
import           System.IO.Unsafe (unsafeDupablePerformIO)
import           XMonad.Util.Run  (runProcessWithInput)

xProperty :: String -> IO String
xProperty key = fromMaybe "" . findValue key <$> runProcessWithInput "xrdb" ["-query"] ""

findValue :: String -> String -> Maybe String
findValue xresKey xres = snd <$> find ((== xresKey) . fst) (catMaybes $ splitAtColon <$> lines xres)

splitAtColon :: String -> Maybe (String, String)
splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

splitAtTrimming :: String -> Int -> (String, String)
splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

trim, xprop :: ShowS
trim = dropWhileEnd isSpace . dropWhile isSpace
xprop = unsafeDupablePerformIO . xProperty
