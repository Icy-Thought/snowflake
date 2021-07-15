{-# LANGUAGE ScopedTypeVariables #-}

{- |
   Module : Theme.Theme
   Copyright : (c) 2021 Joan Milev <joantmilev@gmail.com>
   License : MIT

   Maintainer : Joan Milev <joantmilev@gmail.com>
   Stability : Stable
   Portability : Unknown
-}

module Theme.Theme ( basebg
                   , basefg
                   , basecr
                   , base00
                   , base08
                   , base01
                   , base09
                   , base02
                   , base10
                   , base03
                   , base11
                   , base04
                   , base12
                   , base05
                   , base13
                   , base06
                   , base14
                   , base07
                   , base15
                   , myFont
                   , myFontGTK
                   , myBigFont
                   , myBoldFont
                   , myItalicFont
                   ) where

import           Prelude          (String)
import           Theme.Xresources (xprop)

basebg, basefg, basecr, base00, base08, base01, base09, base02, base10, base03, base11, base04, base12, base05, base13, base06, base14, base07, base15 :: String
basebg = xprop "*.background"
basefg = xprop "*.foreground"
basecr = xprop "*.cursorColor"
base00 = xprop "*.color0"
base08 = xprop "*.color8"
base01 = xprop "*.color1"
base09 = xprop "*.color9"
base02 = xprop "*.color2"
base10 = xprop "*.color10"
base03 = xprop "*.color3"
base11 = xprop "*.color11"
base04 = xprop "*.color4"
base12 = xprop "*.color12"
base05 = xprop "*.color5"
base13 = xprop "*.color13"
base06 = xprop "*.color6"
base14 = xprop "*.color14"
base07 = xprop "*.color7"
base15 = xprop "*.color15"

myFont, myFontGTK,myBigFont, myBoldFont, myItalicFont :: String
myFont = xprop "xmonad.font"
myFontGTK = xprop "xmonad.font.gtk"
myBigFont = xprop "xmonad.font.big"
myBoldFont = xprop "xmonad.font.bold"
myItalicFont = xprop "xmonad.font.italic"
