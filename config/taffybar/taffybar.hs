{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception.Base
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8                       as BS
import           Data.List
import           Data.List.Split
import qualified Data.Map                                    as M
import           Data.Maybe
import qualified Data.Text
import           Data.Time
import qualified GI.Gtk                                      as Gtk
import qualified GI.Gtk.Objects.Overlay                      as Gtk
import           Network.HostName
import           StatusNotifier.Tray
import           System.Directory
import           System.Environment
import           System.Environment.XDG.BaseDir
import           System.FilePath.Posix
import           System.IO
import           System.Log.Handler.Simple
import           System.Log.Logger
import           System.Process
import           System.Taffybar
import           System.Taffybar.Auth
import           System.Taffybar.Context                     (appendHook)
import           System.Taffybar.DBus
import           System.Taffybar.DBus.Toggle
import           System.Taffybar.Hooks
import           System.Taffybar.Information.CPU
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Information.Memory
import           System.Taffybar.Information.X11DesktopInfo
import           System.Taffybar.SimpleConfig
import           System.Taffybar.Util
import           System.Taffybar.Widget
import           System.Taffybar.Widget.Generic.Icon
import           System.Taffybar.Widget.Generic.PollingGraph
import           System.Taffybar.Widget.Generic.PollingLabel
import           System.Taffybar.Widget.Util
import           System.Taffybar.Widget.Workspaces
import           Text.Printf
import           Text.Read                                   hiding (lift)

main = do
    enableLogger "Graphics.UI.GIGtkStrut" DEBUG
    hostName      <- getHostName
    homeDirectory <- getHomeDirectory
    let relativeFiles =
          fromMaybe ["taffybar.css"] $ lookup hostName cssFilesByHostname
    cssFiles <- mapM (getUserConfigFile "taffybar") relativeFiles

    let
        baseEndWidgets   = [myTray, myNet, myMem, myCPU]
        laptopEndWidgets = myBattery ++ baseEndWidgets
        baseConfig       = defaultSimpleTaffyConfig
            { startWidgets  = [myLauncher, myWorkspaces, myLayout]
            , endWidgets    = baseEndWidgets
            , centerWidgets = [myClock]
            , barPosition   = Top
            , widgetSpacing = 0
            , barPadding    = 0
            , barHeight     = ScreenRatio (1 / 24)
            , cssPaths      = cssFiles
            }
        selectedConfig =
          fromMaybe baseConfig $ lookup hostName
            [ ("thinkpad-e595", baseConfig { endWidgets = laptopEndWidgets })
            , ("probook-440g3", baseConfig { endWidgets = laptopEndWidgets })
            ]
        simpleTaffyConfig = selectedConfig
          { centerWidgets = [myClock] }

    startTaffybar
        $ appendHook (void $ getTrayHost False)
        $ withLogServer
        $ withToggleServer
        $ toTaffyConfig simpleTaffyConfig

setClassAndBoundingBoxes
    :: MonadIO m => Data.Text.Text -> Gtk.Widget -> m Gtk.Widget
setClassAndBoundingBoxes klass =
    buildContentsBox >=> flip widgetSetClassGI klass

deocrateWithSetClassAndBoxes
    :: MonadIO m => Data.Text.Text -> m Gtk.Widget -> m Gtk.Widget
deocrateWithSetClassAndBoxes klass builder =
    builder >>= setClassAndBoundingBoxes klass

makeCombinedWidget constructors = do
    widgets <- sequence constructors
    hbox    <- Gtk.boxNew Gtk.OrientationHorizontal 0
    mapM_ (Gtk.containerAdd hbox) widgets
    Gtk.toWidget hbox

mkRGBA (r, g, b, a) = (r / 256, g / 256, b / 256, a / 256)

ctBlue = mkRGBA (122, 162, 247, 256)
ctRed = mkRGBA (247, 118, 142, 256)
ctYellow = mkRGBA (224, 175, 104, 256)

myGraphConfig = defaultGraphConfig
    { graphPadding         = 0
    , graphBorderWidth     = 0
    , graphWidth           = 75
    , graphBackgroundColor = (0.0, 0.0, 0.0, 0.0)
    }

cpuCfg = myGraphConfig { graphDataColors = [ctRed], graphLabel = Just "Cpu" }

memCfg = myGraphConfig { graphDataColors = [ctBlue], graphLabel = Just "Mem" }

netCfg =
    myGraphConfig { graphDataColors = [ctYellow], graphLabel = Just "Net" }

memCallback :: IO [Double]
memCallback = do
    mi <- parseMeminfo
    return [memoryUsedRatio mi]

cpuCallback = do
    (_, systemLoad, totalLoad) <- cpuLoad
    return [totalLoad, systemLoad]

getFullWorkspaceNames :: X11Property [(WorkspaceId, String)]
getFullWorkspaceNames = go
    <$> readAsListOfString Nothing "_NET_DESKTOP_FULL_NAMES"
    where go = zip [ WorkspaceId i | i <- [0 ..] ]

workspaceNamesLabelSetter workspace =
    remapNSP
        .   fromMaybe ""
        .   lookup (workspaceIdx workspace)
        <$> liftX11Def [] getFullWorkspaceNames
    where
        remapNSP "NSP" = "S"
        remapNSP n     = n

enableLogger logger level = do
    logger <- getLogger logger
    saveGlobalLogger $ setLevel level logger

logDebug = do
    global <- getLogger ""
    saveGlobalLogger $ setLevel DEBUG global
    logger3 <- getLogger "System.Taffybar"
    saveGlobalLogger $ setLevel DEBUG logger3
    logger <- getLogger "System.Taffybar.Widget.Generic.AutoSizeImage"
    saveGlobalLogger $ setLevel DEBUG logger
    logger2 <- getLogger "StatusNotifier.Tray"
    saveGlobalLogger $ setLevel DEBUG logger2

cssFilesByHostname =
    [("thinkpad-e595", ["taffybar.css"])
    , ("probook-440g3", ["taffybar.css"])]

myCPU = deocrateWithSetClassAndBoxes "cpu"
    $ pollingGraphNew cpuCfg 5 cpuCallback

myMem = deocrateWithSetClassAndBoxes "mem"
    $ pollingGraphNew memCfg 5 memCallback

myNet = deocrateWithSetClassAndBoxes "net"
    $ networkGraphNew netCfg Nothing

myLayout = deocrateWithSetClassAndBoxes "layout"
    $ layoutNew defaultLayoutConfig

myWindows = deocrateWithSetClassAndBoxes "windows"
    $ windowsNew defaultWindowsConfig

myLauncher = deocrateWithSetClassAndBoxes "launcher"
    $ simpleCommandButtonNew "\62227  NixOS" "rofi -no-lazy-grab -show drun -modi drun"

myWorkspaces = flip widgetSetClassGI "workspaces" =<< workspacesNew
    defaultWorkspacesConfig
        { minIcons                    = 1
        , getWindowIconPixbuf         =
          scaledWindowIconPixbufGetter
          $     getWindowIconPixbufFromChrome
          <|||> unscaledDefaultGetWindowIconPixbuf
          <|||> (\size _ -> lift $ loadPixbufByName size "application-default-icon")
        , widgetGap                   = 0
        , showWorkspaceFn             = hideEmpty
        , updateRateLimitMicroseconds = 100000
        , labelSetter                 = myLabelSetter -- workspaceNamesLabelSetter
        , widgetBuilder               = buildLabelOverlayController
        }

myLabelSetter workspace = return
    $ case workspaceName workspace of
        "1" -> "一"
        "2" -> "二"
        "3" -> "三"
        "4" -> "四"
        "5" -> "五"
        "6" -> "六"
        "7" -> "七"
        "8" -> "八"
        "9" -> "九"
        n   -> n

myClock =
    deocrateWithSetClassAndBoxes "clock"
        $ textClockNewWith defaultClockConfig
            { clockUpdateStrategy = RoundedTargetInterval 60 0.0
            , clockFormatString   = "\61463  %H:%M   \61555  %d/%m/%y"
            }

myTray =
    deocrateWithSetClassAndBoxes "tray"
        $ sniTrayNewFromParams defaultTrayParams
            { trayRightClickAction = PopupMenu
            , trayLeftClickAction  = Activate
            }

myBattery =
    [ deocrateWithSetClassAndBoxes "battery" $ makeCombinedWidget
        [textBatteryNew "$percentage$% ", batteryIconNew]
    ]
