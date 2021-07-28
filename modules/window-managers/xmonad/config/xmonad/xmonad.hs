{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Main (main) where

import           Control.Monad (liftM2)
import qualified Data.Map as M
import           Data.Monoid (All)
import           System.Exit (exitSuccess)
import           Theme.Theme                         (base00, base01, base04,
                                                      base05, base06, base07,
                                                      basebg, basefg, myFont,
                                                      myFontGTK)
import           XMonad
import           XMonad.Actions.CopyWindow           (copyToAll, kill1,
                                                      killAllOtherCopies)
import           XMonad.Actions.CycleWS              (Direction1D (Next, Prev),
                                                      WSType (Not, (:&:)),
                                                      emptyWS, hiddenWS,
                                                      ignoringWSs, moveTo,
                                                      shiftTo, toggleWS')
import qualified XMonad.Actions.FlexibleResize as Flex
import           XMonad.Actions.NoBorders (toggleBorder)
import           XMonad.Actions.Promote (promote)
import           XMonad.Actions.Submap (submap)
import           XMonad.Actions.TiledWindowDragging (dragWindow)
import           XMonad.Actions.WithAll (killAll, sinkAll)
import           XMonad.Hooks.DynamicIcons           (IconConfig (..), appIcon,
                                                      dynamicIconsPP,
                                                      iconsFmtReplace,
                                                      iconsGetFocus,
                                                      wrapUnwords)
import           XMonad.Hooks.EwmhDesktops           (activateLogHook, ewmh,
                                                      ewmhFullscreen)
import           XMonad.Hooks.ManageDocks            (ToggleStruts (ToggleStruts),
                                                      avoidStruts, docks)
import           XMonad.Hooks.ManageHelpers          (composeOne, doCenterFloat,
                                                      doFullFloat, isDialog,
                                                      isFullscreen, transience,
                                                      (-?>))
import           XMonad.Hooks.StatusBar              (StatusBarConfig,
                                                      statusBarProp, withSB)
import           XMonad.Hooks.StatusBar.PP           (PP (..), filterOutWsPP,
                                                      shorten', wrap,
                                                      xmobarAction,
                                                      xmobarBorder, xmobarColor,
                                                      xmobarFont, xmobarStrip)
import           XMonad.Hooks.WindowSwallowing (swallowEventHook)
import           XMonad.Layout.Accordion (Accordion (Accordion))
import           XMonad.Layout.DraggingVisualizer (draggingVisualizer)
import           XMonad.Layout.LayoutModifier (ModifiedLayout)
import           XMonad.Layout.MultiToggle           (EOT (EOT),
                                                      Toggle (Toggle), mkToggle,
                                                      (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers (NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders (smartBorders)
import           XMonad.Layout.Renamed (Rename (Replace), renamed)
import           XMonad.Layout.ResizableThreeColumns (ResizableThreeCol (ResizableThreeColMid))
import           XMonad.Layout.ResizableTile         (MirrorResize (MirrorExpand, MirrorShrink),
                                                      ResizableTall (ResizableTall))
import           XMonad.Layout.Simplest (Simplest (Simplest))
import           XMonad.Layout.Spacing               (Border (Border), Spacing,
                                                      decScreenSpacing,
                                                      decScreenWindowSpacing,
                                                      decWindowSpacing,
                                                      incScreenSpacing,
                                                      incScreenWindowSpacing,
                                                      incWindowSpacing,
                                                      spacingRaw,
                                                      toggleScreenSpacingEnabled,
                                                      toggleWindowSpacingEnabled)
import           XMonad.Layout.SubLayouts            (GroupMsg (MergeAll, UnMerge),
                                                      onGroup, pullGroup,
                                                      subLayout, toSubl)
import           XMonad.Layout.Tabbed                (Direction2D (D, L, R, U),
                                                      Theme (..), addTabs,
                                                      shrinkText)
import           XMonad.Layout.WindowNavigation (windowNavigation)
import           XMonad.Main (launch)
import           XMonad.Prompt                       (XPConfig (..),
                                                      XPPosition (Top),
                                                      defaultXPKeymap,
                                                      deleteAllDuplicates)
import           XMonad.Prompt.ConfirmPrompt (confirmPrompt)
import           XMonad.Prompt.FuzzyMatch (fuzzyMatch, fuzzySort)
import           XMonad.Prompt.Man (manPrompt)
import           XMonad.Prompt.Shell (shellPrompt)
import qualified XMonad.StackSet as W
import           XMonad.Util.ClickableWorkspaces (clickablePP)
import           XMonad.Util.Cursor (setDefaultCursor)
import           XMonad.Util.DynamicScratchpads      (makeDynamicSP,
                                                      spawnDynamicSP)
import           XMonad.Util.NamedScratchpad         (NamedScratchpad (NS),
                                                      customFloating,
                                                      namedScratchpadAction,
                                                      namedScratchpadManageHook,
                                                      scratchpadWorkspaceTag)
import           XMonad.Util.Run (safeSpawn, unsafeSpawn)
import           XMonad.Util.SpawnOnce (spawnOnce)
import           XMonad.Util.Ungrab (unGrab)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal :: String
myTerminal = "alacritty"

-- My launcher
--
myLauncher :: String
myLauncher = "rofi -theme ~/.config/rofi/themes/slate.rasi -width 624 -lines 12"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth :: Dimension
myBorderWidth = 1

myGaps :: Num p => p
myGaps = 8

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask, altMask :: KeyMask
myModMask = mod4Mask
altMask = mod1Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces :: [WorkspaceId]
myWorkspaces =
  [ "\xf8a3"
  , "\xf8a6"
  , "\xf8a9"
  , "\xf8ac"
  , "\xf8af"
  , "\xf8b2"
  , "\xf8b5"
  , "\xf8b8"
  , "\xf8bb"
  ]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor, myFocusedBorderColor :: String
myNormalBorderColor = base00
myFocusedBorderColor = base04

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig { XMonad.modMask = modm } =
  M.fromList
    $  [
       -- launch a terminal
         ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

       -- launch dmenu
       --, ((altMask, xK_p), spawn "dmenu_run -p 'Run:' -w 1916")

       -- launch greenclip-dmenu
       --, ((altMask, xK_c), spawn "greenclip print | sed '/^$/d' | dmenu -s -l 10 -g 2 -w 1916 -p 'Clipboard:' | xargs -r -d'\n' -I '{}' greenclip print '{}'")

       -- launch rofi
       , ( (altMask, xK_p)
         , unsafeSpawn
           (  myLauncher
           ++ " -show combi -combi-modi window,drun -modi combi -show-icons"
           )
         )

       -- launch rofi-greenclip
       , ( (modm, xK_c)
         , unsafeSpawn
           (myLauncher
           ++ " -show '\xf0ea Clipboard' -modi '\xf0ea Clipboard:greenclip print' -run-command '{cmd}'"
           )
         )

       -- launch gmrun
       -- , ((modm .|. shiftMask, xK_p )   , safeSpawn "gmrun" [])

       -- close focused window
       , ((modm .|. shiftMask, xK_c)    , kill1)

       -- close all windows on the current workspace
       , ((modm .|. controlMask, xK_c)  , killAll)

       -- Rotate through the available layout algorithms
       , ((modm, xK_space)              , sendMessage NextLayout)

       --  Reset the layouts on the current workspace to default
       , ((modm .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

       -- Resize viewed windows to the correct size
       , ((modm, xK_n)                  , refresh)

       -- Move focus to the next window
       , ((modm, xK_j)                  , windows W.focusDown)

       -- Move focus to the previous window
       , ((modm, xK_k)                  , windows W.focusUp)

       -- Move focus to the master window
       , ((modm, xK_m)                  , windows W.focusMaster)

       -- Swap the focused window and the master window
       -- , ((modm, xK_Return)             , windows W.swapMaster)

       -- Swap the focused window with the next window
       , ((modm .|. shiftMask, xK_j)    , windows W.swapDown)

       -- Swap the focused window with the previous window
       , ((modm .|. shiftMask, xK_k)    , windows W.swapUp)

       -- Shrink the master area
       , ((modm, xK_h)                  , sendMessage Shrink)

       -- Expand the master area
       , ((modm, xK_l)                  , sendMessage Expand)

       -- Shrink focused windows height
       , ((modm .|. altMask, xK_j)      , sendMessage MirrorShrink)

       -- Expand focused windows height
       , ((modm .|. altMask, xK_k)      , sendMessage MirrorExpand)

       -- Push window back into tiling
       , ((modm, xK_t)                  , withFocused $ windows . W.sink)

       -- Push all windows on the current workspace into tiling
       , ((modm .|. shiftMask, xK_t)    , sinkAll)

       -- Increment the number of windows in the master area
       , ((modm, xK_comma)              , sendMessage (IncMasterN 1))

       -- Deincrement the number of windows in the master area
       , ((modm, xK_period)             , sendMessage (IncMasterN (-1)))

       -- Toggle the status bar gap
       -- Use this binding with avoidStruts from Hooks.ManageDocks.
       -- See also the statusBar function from Hooks.DynamicLog.
       --
       , ((modm .|. controlMask, xK_b)  , sendMessage ToggleStruts)

       -- Quit xmonad
       , ((modm .|. shiftMask, xK_q), confirmPrompt myXPConfig "Quit" $ io exitSuccess)

       -- Restart xmonad
       , ((modm, xK_q), unsafeSpawn "xmonad --recompile; xmonad --restart")

       -- Moves the focused window to the master pane
       , ((modm, xK_Return), promote)

       -- Run xmessage with a summary of the default keybindings (useful for beginners)
       , ( (modm .|. shiftMask, xK_slash)
         , unsafeSpawn ("printf \"" ++ help ++ "\" | gxmessage -title 'XMonad Keybind' -fn '" ++ myFontGTK ++ "' -file -")
         )
       -- Toggle border on focused window
       , ((modm, xK_b)                  , withFocused toggleBorder)

       -- Toggle all borders
       , ((modm .|. shiftMask, xK_b)    , sendMessage $ Toggle NOBORDERS)

       -- Toggle Fullscreen
       , ((modm, xK_f)                  , sendMessage $ Toggle NBFULL)

       -- CycleWS setup
       , ((modm, xK_Right)              , moveTo Next nonNSP)
       , ((modm, xK_Left)               , moveTo Prev nonNSP)
       , ((modm, xK_Tab)                , moveTo Next nonEmptyNSP)
       , ((modm .|. shiftMask, xK_Tab)  , moveTo Prev nonEmptyNSP)
       , ((modm .|. shiftMask, xK_Right), shiftTo Next nonNSP)
       , ((modm .|. shiftMask, xK_Left) , shiftTo Prev nonNSP)
       , ((modm, xK_z)                  , toggleWS' [scratchpadWorkspaceTag])
       , ((modm .|. shiftMask, xK_f)    , moveTo Next emptyWS)

       -- Increase/Decrease spacing (gaps)
       , ( (modm, xK_g)
         , sequence_ [toggleScreenSpacingEnabled, toggleWindowSpacingEnabled]
         )
       , ((modm, xK_i)                     , incScreenWindowSpacing 2)
       , ((modm, xK_d)                     , decScreenWindowSpacing 2)
       , ((altMask, xK_i)                  , incScreenSpacing 2)
       , ((altMask, xK_d)                  , decScreenSpacing 2)
       , ((altMask .|. shiftMask, xK_i)    , incWindowSpacing 2)
       , ((altMask .|. shiftMask, xK_d)    , decWindowSpacing 2)

       -- SubLayouts
       , ((modm .|. controlMask, xK_h)     , sendMessage $ pullGroup L)
       , ((modm .|. controlMask, xK_l)     , sendMessage $ pullGroup R)
       , ((modm .|. controlMask, xK_k)     , sendMessage $ pullGroup U)
       , ((modm .|. controlMask, xK_j)     , sendMessage $ pullGroup D)
       , ((modm .|. controlMask, xK_space) , toSubl NextLayout)
       , ((modm .|. controlMask, xK_m), withFocused (sendMessage . MergeAll))
       , ((modm .|. controlMask, xK_u), withFocused (sendMessage . UnMerge))
       , ((modm .|. controlMask, xK_comma) , onGroup W.focusUp')
       , ((modm .|. controlMask, xK_period), onGroup W.focusDown')

       -- Copy window
       , ((modm, xK_v)                     , windows copyToAll)
       , ((modm .|. shiftMask, xK_v)       , killAllOtherCopies)

       -- Scratchpad
       , ( (modm .|. controlMask, xK_Return)
         , namedScratchpadAction myScratchpads "terminal"
         )

       -- Dynamic scratchpads
       , ((modm .|. shiftMask, xK_equal), withFocused $ makeDynamicSP "dyn1")
       , ((modm .|. shiftMask, xK_minus), withFocused $ makeDynamicSP "dyn2")
       , ((modm, xK_equal)              , spawnDynamicSP "dyn1")
       , ((modm, xK_minus)              , spawnDynamicSP "dyn2")

       -- Easily switch your layouts
       , ((altMask, xK_t)               , sendMessage $ JumpToLayout "Tall")
       , ((altMask, xK_c), sendMessage $ JumpToLayout "Centered Master")

       -- XPrompt
       , ((modm, xK_p)                  , shellPrompt myXPConfig)
       , ((modm, xK_F1)                 , manPrompt myXPConfig)

       -- mpd music control
       , ((modm, xK_a), submap . M.fromList $
           [ ((0, xK_n),     safeSpawn "mpc" ["next"])
           , ((0, xK_p),     safeSpawn "mpc" ["prev"])
           , ((0, xK_s),     safeSpawn "mpd" ["stop"])
           , ((0, xK_z),     safeSpawn "mpc" ["random"])
           , ((0, xK_space), safeSpawn "mpc" ["toggle"])
           ])

       -- Open apps
       , ( (altMask, xK_F9)
         , unGrab
           *> unsafeSpawn
                "$(killall picom && notify-send -u critical -i picom 'System' 'Killed Picom') || $(picom & notify-send -u critical -i picom 'System' 'Picom running...')"
         )
       , ((altMask, xK_e), safeSpawn "emacsclient" ["-nc"])
       , ((altMask, xK_b), safeSpawn "firefox" [])

       -- lock screen
       , ( (modm .|. shiftMask, xK_l)
         , unGrab *> safeSpawn "loginctl" ["lock-session"]
         )

       -- Screenshot shortcuts (Requires: shotgun, slop, xdotool)
       , ((0, xK_Print)                , unGrab *> safeSpawn "shotclip" ["-f"])
       , ((0 .|. controlMask, xK_Print), unGrab *> safeSpawn "shotclip" ["-w"])
       , ((0 .|. shiftMask, xK_Print)  , unGrab *> safeSpawn "shotclip" ["-s"])
       ]
    ++
       --
       -- mod-[1..9], Switch to workspace N
       -- mod-shift-[1..9], Move client to workspace N
       --
       [ ((m .|. modm, k), windows $ f i)
       | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
       , (f, m) <-
         [ (W.greedyView                   , 0)
         , (W.shift                        , shiftMask)
         , (liftM2 (.) W.greedyView W.shift, controlMask)
         ]
       ]
    ++
       --
       -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
       -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
       --
       [ ((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0 ..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
       ]
 where
  nonNSP = ignoringWSs [scratchpadWorkspaceTag]
  nonEmptyNSP =
    hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings XConfig { XMonad.modMask = modm } = M.fromList
  -- mod-button1, Set the window to floating mode and move by dragging
  [ ( (modm, button1)
    , \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster
    )

  -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), \w -> focus w >> windows W.shiftMaster)

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ( (modm, button3)
    , \w -> focus w >> Flex.mouseResizeWindow w >> windows W.shiftMaster
    )

  -- scroll the mouse wheel (button4 and button5)
  , ((modm, button4)              , \w -> focus w >> moveTo Prev nonNSP)
  , ((modm, button5)              , \w -> focus w >> moveTo Next nonNSP)

  -- drag windows
  , ((modm .|. shiftMask, button1), dragWindow)
  ]
  where nonNSP = ignoringWSs [scratchpadWorkspaceTag]

------------------------------------------------------------------------
-- XPrompt
--
myXPConfig :: XPConfig
myXPConfig = def { font                = myFont
                 , bgColor             = basebg
                 , fgColor             = basefg
                 , bgHLight            = base04
                 , fgHLight            = basebg
                 , borderColor         = base00
                 , promptBorderWidth   = 1
                 , promptKeymap        = defaultXPKeymap
                 , position            = Top
                 -- , position            = CenteredAt {xpCenterY = 0.3, xpWidth = 0.3}
                 , height              = 30
                 , historySize         = 50
                 , historyFilter       = deleteAllDuplicates
                 , defaultText         = []
                 -- , autoComplete        = Just 100000,   -- set Just 100000 for .1 sec
                 , showCompletionOnTab = False          -- False means auto completion
                 , alwaysHighlight     = True           -- Disables tab cycle
                 , maxComplRows        = Just 10        -- set to 'Just 5' for 5 rows
                 , searchPredicate     = fuzzyMatch
                 , sorter              = fuzzySort
                 }

------------------------------------------------------------------------
-- Tab theme
myTabConfig :: Theme
myTabConfig = def { activeColor         = base04
                  , activeBorderColor   = base00
                  , activeTextColor     = basebg
                  , inactiveColor       = basebg
                  , inactiveBorderColor = basebg
                  , inactiveTextColor   = base00
                  , fontName            = myFont
                  , decoHeight          = 22
                  , decoWidth           = maxBound
                  }

-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

myLayout =
  avoidStruts . smartBorders . mkToggle (NOBORDERS ?? NBFULL ?? EOT) $ myLayouts
 where
  -- default tiling algorithm partitions the screen into two panes
  myLayouts = tall ||| horizon ||| threeCol ||| monocle

  -- my layouts
  tall = rn "Tall" . mySpacing myGaps . mkTabbed . dragWindows $ ResizableTall
    nmaster
    delta
    ratio
    []

  horizon = rn "Horizon" . mySpacing myGaps . mkTabbed . dragWindows $ Mirror
    (ResizableTall nmaster delta ratio [])

  threeCol =
    rn "ThreeCol" . mySpacing myGaps . mkTabbed . dragWindows $ ResizableThreeColMid
      nmaster
      delta
      ratio
      []

  monocle = rn "Monocle" . mySpacing myGaps $ Full

  -- Simplify things
  rn n = renamed [Replace n]
  mkTabbed layout =
    addTabs shrinkText myTabConfig
      . subLayout [] (Simplest ||| Accordion)
      $ layout
  dragWindows layout = windowNavigation . draggingVisualizer $ layout

  -- gaps
  mySpacing :: Integer -> l a -> ModifiedLayout XMonad.Layout.Spacing.Spacing l a
  mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

  -- The default number of windows in the master pane
  nmaster = 1

  -- Default proportion of screen occupied by master pane
  ratio   = 1 / 2

  -- Percent of screen to increment by when resizing panes
  delta   = 3 / 100

------------------------------------------------------------------------
-- Scratchpad
--
myScratchpads :: [NamedScratchpad]
myScratchpads = [
                 -- run a terminal inside scratchpad
                 NS "terminal" spawnTerm findTerm manageTerm]
 where
  spawnTerm  = myTerminal ++ " -c scratchpad"
  findTerm   = className =? "scratchpad"
  manageTerm = customFloating $ W.RationalRect (1 / 6) (1 / 8) (2 / 3) (3 / 4)

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
-- workspace number starts from 0
--

myManageHook :: ManageHook
myManageHook =
  composeOne
      [ className =? "MPlayer" -?> doFloat
      , resource =? "desktop_window" -?> doIgnore
      , resource =? "kdesktop" -?> doIgnore
      , resource =? "Toolkit" <||> resource =? "Browser" -?> doFloat
      , resource =? "redshift-gtk" -?> doCenterFloat
      , className =? "Gammastep-indicator" -?> doCenterFloat
      , className =? "ibus-ui-gtk3" -?> doIgnore
      , resource =? "gcr-prompter" -?> doCenterFloat
      , className =? "St-float" -?> doFloat
      , transience
      , title =? "XMonad Keybind" -?> doCenterFloat
      , className =? "Ibus-extension-gtk3" -?> doFloat
      , isFullscreen -?> doFullFloat
      , isDialog -?> doCenterFloat
      , className =? "firefox" -?> doShift (myWorkspaces !! 1)
      , className =? "discord" -?> doShift (myWorkspaces !! 2)
      , className =? "code-oss" -?> doShift (myWorkspaces !! 3)
      , className =? "Lutris" -?> doShift (myWorkspaces !! 5)
      , className =? "VirtualBox Manager" <||> className =? "gnome-boxes" -?>  doShift (myWorkspaces !! 6)
      ]
    <+> namedScratchpadManageHook myScratchpads

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook

--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myHandleEventHook :: Event -> X All
myHandleEventHook = handleEventHook def <+> swallowEventHook
  (    className
  =?   "Alacritty"
  <||> className
  =?   "St"
  <||> className
  =?   "org.wezfurlong.wezterm"
  <||> className
  =?   "kitty"
  )
  (    (   not
       <$> (    className
           =?   "St"
           <||> className
           =?   "St-float"
           <||> className
           =?   "kitty"
           <||> className
           =?   "org.wezfurlong.wezterm"
           <||> className
           =?   "Dragon"
           <||> className
           =?   "qemu-system-x86_64"
           <||> className
           =?   "noswallow"
           )
       )
  <||> className
  =?   "re"
  )

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
--
-- myLogHook :: X ()
-- myLogHook = refocusLastLogHook >> nsHideOnFocusLoss myScratchpads

mySB :: StatusBarConfig
mySB = statusBarProp
  "xmobar"
  (clickablePP =<< dynamicIconsPP myIconConfig (filterOutWsPP [scratchpadWorkspaceTag] myXmobarPP))
 where
  myXmobarPP :: PP
  myXmobarPP = def
    { ppSep           = wrapSep " "
    , ppTitleSanitize = xmobarStrip
    , ppCurrent       = blue . wrap "" "" . xmobarBorder "Bottom" base06 2 . xmobarFont 2
    , ppHidden        = lowWhite . wrap "" "" . xmobarFont 2
    , ppWsSep         = xmobarColor "" background "  "
    , ppTitle         = magenta . xmobarAction "xdotool key Super+shift+c" "2" . shorten 40
    -- , ppOrder         = \[ws, l, t, ex] -> [ws, l, ex, t]
    -- , ppExtras        = [xmobarColorL base01 background windowCount]
    , ppLayout        = red . xmobarAction "xdotool key Super+space" "1" . xmobarAction
                            "xdotool key Super+shift+space"
                            "3"
                            . (\case
                                "Tall"     -> "<icon=Tall.xpm/>"
                                "Horizon"  -> "<icon=Horizon.xpm/>"
                                "ThreeCol" -> "<icon=ThreeCol.xpm/>"
                                "Monocle"  -> "<icon=Monocle.xpm/>"
                                _          -> "?"
                              )
    }
   where
    shorten :: Int -> String -> String
    shorten = shorten' "…"

    wrapSep :: String -> String
    wrapSep = wrap (xmobarColor base00 "" (xmobarFont 5 "\xe0b4"))
                   (xmobarColor base00 "" (xmobarFont 5 "\xe0b6"))

    background :: String
    background = base00 ++ ":5"

    blue, lowWhite, magenta, red :: String -> String
    magenta  = xmobarColor base05 background
    blue     = xmobarColor base04 background
    -- purple   = xmobarColor "#bd93f9" "#2c323a:5"
    -- lowBlue  = xmobarColor "#8be9fd" "#2c323a:5"
    -- white    = xmobarColor "#f8f8f2" "#2c323a:5"
    -- yellow   = xmobarColor "#f1fa8c" "#2c323a:5"
    red      = xmobarColor base01 background
    lowWhite = xmobarColor base07 background
    -- gray     = xmobarColor "" background
    -- green    = xmobarColor base02 background

    -- -- Get count of available windows on a workspace
    -- windowCount :: X (Maybe String)
    -- windowCount =
    --   gets
    --     $ Just
    --     . show
    --     . length
    --     . W.integrate'
    --     . W.stack
    --     . W.workspace
    --     . W.current
    --     . windowset

  myIconConfig :: IconConfig
  myIconConfig = def { iconConfigIcons  = myIcons
                     , iconConfigFmt    = iconsFmtReplace (wrapUnwords "" "")
                     , iconConfigFilter = iconsGetFocus
                     }
   where
    myIcons :: Query [String]
    myIcons = composeAll
      [ className =? "discord" --> appIcon "<fn=3>\xf392</fn>"
      , className =? "Discord" --> appIcon "<fn=3>\xf268</fn>"
      , className =? "firefox" --> appIcon "<fn=3>\xf269</fn>"
      , className =? "Brave-browser" --> appIcon "<fn=3>\xf268</fn>"
      , className =? "St" --> appIcon "<fn=2>\xe795</fn>"
      , className =? "Emacs" --> appIcon "<fn=4>\xe926</fn>"
      , className =? "code-oss" --> appIcon "<fn=4>\xe60c</fn>"
      , className =? "Org.gnome.Nautilus" --> appIcon "<fn=1>\xf07b</fn>"
      , className =? "Spotify" --> appIcon "<fn=3>\xf1bc</fn>"
      , className =? "mpv" --> appIcon "<fn=1>\xf03d</fn>"
      , className =? "VirtualBox Manager" --> appIcon "<fn=4>\xea3e</fn>"
      , className =? "Lutris" --> appIcon "<fn=1>\xf11b</fn>"
      , className =? "Sxiv" --> appIcon "<fn=1>\xf03e</fn>"
      ]

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
myStartupHook :: X ()
myStartupHook = do
  setDefaultCursor xC_left_ptr
  spawnOnce "wall"
  -- spawnOnce "xwallpaper --zoom ~/Pictures/macOS-Big-Sur-night.jpg"
  -- spawnOnce "feh --no-fehbg --bg-scale ~/Pictures/Wallpapers/0057.jpg"
  -- spawn "feh --bg-scale --randomize --no-fehbg ~/Pictures/Wallpapers/*"
  spawnOnce "picom"
  spawnOnce "dunst"
  spawnOnce "nm-applet"
  -- spawnOnce "redshift-gtk"
  spawnOnce "gammastep-indicator"
  spawnOnce "greenclip daemon"
  spawnOnce "numlockx"
  -- spawnOnce "emacs --daemon"
  spawnOnce
    "dbus-launch --exit-with-session ~/.local/share/xmonad/xmonad-x86_64-linux"
  spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1"
  spawnOnce "ibus-daemon -x"
  spawnOnce "mpd --no-daemon"
  spawnOnce "mpDris2"
  spawnOnce "playerctld"
  spawnOnce "xss-lock -- lockctl -t 30 -l"
  spawnOnce
    ("stalonetray --geometry 1x1-17+5 --max-geometry 10x1-17+5 --transparent --tint-color '"
    ++ base00
    ++ "' --tint-level 255 --grow-gravity NE --icon-gravity NW --icon-size 20 --sticky --window-type dock --window-strut top --skip-taskbar"
    )
  -- spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x2c323a  --height 22 --iconspacing 5 --distance 2,2 --distancefrom top,right"

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main :: IO ()
main = do
  let acMh :: ManageHook
      acMh = reader W.focusWindow >>= doF
  dirs <- getDirectories
  (flip launch dirs) . withSB mySB . ewmhFullscreen . ewmh . docks $ def
    {
      -- simple stuff
      terminal           = myTerminal
    , focusFollowsMouse  = myFocusFollowsMouse
    , clickJustFocuses   = myClickJustFocuses
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , workspaces         = myWorkspaces
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor

      -- key bindings
    , keys               = myKeys
    , mouseBindings      = myMouseBindings

      -- hooks, layouts
    , layoutHook         = myLayout
    , manageHook         = myManageHook
    , handleEventHook    = myHandleEventHook
    , logHook            = activateLogHook acMh <+> logHook def
    , startupHook        = myStartupHook
    }

------------------------------------------------------------------------

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines
  [ "The default modifier key is 'super'. Default keybindings:"
  , ""
  , "-- launching and killing programs"
  , "mod-Shift-Enter      Launch terminal"
  , "mod-p                Launch XPrompt (Xmonad Prompt)"
  , "mod-c                Launch greenclip with rofi"
  --, "Alt-p                Launch dmenu"
  --, "Alt-c                Launch greenclip with dmenu"
  , "mod-Shift-c          Close/kill the focused window"
  , "mod-Space            Rotate through the available layout algorithms"
  , "mod-Shift-Space      Reset the layouts on the current workSpace to default"
  , "mod-n                Resize/refresh viewed windows to the correct size"
  , ""
  , "-- move focus up or down the window stack"
  , "mod-Tab              Move focus to the next window"
  , "mod-Shift-Tab        Move focus to the previous window"
  , "mod-j                Move focus to the next window"
  , "mod-k                Move focus to the previous window"
  , "mod-m                Move focus to the master window"
  , ""
  , "-- modifying the window order"
  , "mod-Return           Move the focused window to the master pane."
  , "mod-Shift-j          Swap the focused window with the next window"
  , "mod-Shift-k          Swap the focused window with the previous window"
  , ""
  , "-- resizing the master/slave ratio"
  , "mod-h                Shrink the master width"
  , "mod-l                Expand the master width"
  , "mod-a                Shrink the master height"
  , "mod-s                Expand the master height"
  , ""
  , "-- increase or decrease spacing (gaps)"
  , "mod-g                Toggle spacing/gaps"
  , "mod-i                Increment both screen and window borders"
  , "mod-d                Deincrement both screen and window borders"
  , "Alt-i                Increment screen borders"
  , "Alt-d                Deincrement screen borders"
  , "Alt-Shift-i          Increment window borders"
  , "Alt-Shift-d          Deincrement window borders"
  , ""
  , "-- floating layer support"
  , "mod-t                Push window back into tiling; unfloat and re-tile it"
  , ""
  , "-- increase or decrease number of windows in the master area"
  , "mod-comma  (mod-,)   Increment the number of windows in the master area"
  , "mod-period (mod-.)   Deincrement the number of windows in the master area"
  , ""
  , "-- quit, or restart"
  , "mod-Shift-q          Quit xmonad"
  , "mod-q                Restart xmonad"
  , ""
  , "-- Workspaces & screens"
  , "mod-[1..9]           Switch to workSpace N"
  , "mod-Shift-[1..9]     Move client to workspace N"
  , "mod-Control-[1..9]   Move client and switch to workspace N"
  , "mod-{w,e,r}          Switch to physical/Xinerama screens 1, 2, or 3"
  , "mod-Shift-{w,e,r}    Move client to screen 1, 2, or 3"
  , "mod-Right            Switch to next workSpace"
  , "mod-Left             Switch to previous workSpace"
  , "mod-Shift-Right      Move client to next workSpace"
  , "mod-Shift-Left       Move client to previous workSpace"
  , "mod-f                Switch to a free workSpace"
  , "mod-z                Switch between previously used workSpace"
  , ""
  , "-- Cycle Workspaces"
  , "mod-Tab              Cycle between active workspaces from left to right"
  , "mod-Shift-Tab        Cycle between active workspaces from right to left"
  , "mod-left             Go to next workspace"
  , "mod-right            Go to preview workspace"
  , "mod-Shift-left       Move focused window to next workspace"
  , "mod-Shift-right      Move focused window to previous workspace"
  , ""
  , "-- Mouse bindings: default actions bound to mouse events"
  , "mod-button1          Set the window to floating mode and move by dragging"
  , "mod-button2          Raise the window to the top of the stack"
  , "mod-button3          Set the window to floating mode and resize by dragging"
  , ""
  , "-- Switch layouts"
  , "Alt-t                Switch to 'Tall' layout"
  , "Alt-c                Switch to 'ThreeColMid' layout"
  , "Alt-f                Switch to 'Full' layout"
  , ""
  , "-- Sublayout bindings"
  , "mod-Ctrl-h           Merge with left client"
  , "mod-Ctrl-l           Merge with right client"
  , "mod-Ctrl-k           Merge with upper client"
  , "mod-Ctrl-j           Merge with lower client"
  , "mod-Ctrl-Space       Switch to next sublayout"
  , "mod-Ctrl-m           Merge all available clients on the workspace"
  , "mod-Ctrl-u           Unmerge currently focused client"
  , "mod-Ctrl-period (.)  Move focus to the next window in the sublayout"
  , "mod-Ctrl-comma (,)   Move focus to the previous window in the sublayout"
  , ""
  , "-- Copy windows"
  , "mod-v                Copy focused window to all workspaces"
  , "mod-Shift-v          Only keep currently focused copied window"
  , ""
  , "-- Scratchpad"
  , "mod-Ctrl-Enter       Open a terminal in scratchpad"
  , "mod-Shift-Equal (=)  Add currently focused window to scratchpad (1)"
  , "mod-Equal (=)        Open scratchpad (1)"
  , "mod-Shift-minus (-)  Add currently focused window to scratchpad (2)"
  , "mod-minus (-)        Open scratchpad (2)"
  , ""
  , "-- Shortcuts for taking screenshots"
  , "Print                Take fullscreen screenshot"
  , "Shift-Print          Take screenshot of selected screen"
  , "Ctrl-Print           Take screenshot of focused window"
  , ""
  , "-- Application"
  -- , "All-e                Open emacs-client"
  , "Alt-F9               Turn on/off picom"
  ]

--- vim:ft=haskell:expandtab
