module Gibil.Layouts where

-- Layouts
-- General
import           XMonad                  hiding ( (|||) )
import           XMonad.Actions.MouseResize     ( mouseResize )
import           XMonad.Layout.LayoutCombinators
                                                ( (|||) )
-- Aesthetics
import           XMonad.Layout.Magnifier hiding ( Toggle )
import           XMonad.Layout.MultiColumns
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import qualified XMonad.Layout.Renamed         as RN
import           XMonad.Layout.Spacing
import           XMonad.Layout.SubLayouts
import           XMonad.Layout.Tabbed
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowArranger
import           XMonad.Layout.WindowNavigation

-- Initialization!
layoutModifiers =
  avoidStruts
    . mouseResize
    . WindowArrange
    . smartBorders
    . WindowNavigation
    . toggleLayouts
    . mkToggle (NBFULL ?? NOBORDERS ?? EOT)

-- FIXME
rename newName = RN.renamed [RN.Replace newName]

layoutInfo =
  layoutModifiers (rename "2 Columns" (Tall 1 (3 / 100) (1 / 2)))
    |||! rename "Large Main" (Tall 1 (3 / 100) (3 / 4))
    |||! rename "3 Columns"  (multiCol [1, 1] 2 0.01 (-0.5))
    |||! myTabbed
  where myTabbed = rename "Tabbed" $ tabbed shrinkText icyTheme
