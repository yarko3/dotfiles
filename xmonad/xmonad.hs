import Control.Applicative
import System.Exit
import System.IO (hPutStrLn, Handle)
import XMonad
import XMonad.Actions.DynamicProjects
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SpawnOn
import XMonad.Core (WorkspaceId)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Fullscreen
import XMonad.Layout.IndependentScreens
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.Run(spawnPipe)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

myTerminal = "gnome-terminal"

restartCmd = "if type xmonad; then xmonad --recompile && \
              \xmonad --restart; else xmessage xmonad not in PATH; fi"

--  ===========================================================================
--                             Workspaces
--  ===========================================================================

wsWORK_TERM    = "WORK_TERM"
wsWORK_BROWSER = "WORK_BROWSER"
wsMEDIA        = "MEDIA"
wsSCRATCH      = "SCRATCH"
wsDOTFILES     = "DOTFILES"

myWorkspaces = [wsWORK_TERM, wsWORK_BROWSER, wsMEDIA, wsSCRATCH]

projects :: [Project]
projects =
    [ Project   { projectName      = wsWORK_TERM
                , projectDirectory = "~/"
                , projectStartHook = Just $ do spawn myTerminal
            }

    , Project   { projectName       = wsWORK_BROWSER
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsWORK_BROWSER "google-chrome m/"
            }

    , Project   { projectName       = wsMEDIA
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsMEDIA "google-chrome --new-window news.ycombinator.com"
                                                spawnOn wsMEDIA "spotify"
            }

    , Project   { projectName       = wsSCRATCH
                , projectDirectory  = "~/"
                , projectStartHook  = Nothing
            }

    , Project   { projectName       = wsDOTFILES
                , projectDirectory  = "~/.dotfiles"
                , projectStartHook  = Just $ do spawn myTerminal
        }
    ]

myLayout = avoidStruts (
    ThreeColMid 1 (3/100) (1/2) |||
    Tall 1 (3/100) (1/2) |||
    Mirror (Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    Full |||
    spiral (6/7)) |||
    noBorders (fullscreenFull Full)

myManageHook = composeAll
    [isFullscreen --> (doF W.focusDown <+> doFullFloat)]

--  ===========================================================================
--                              Mappings
--  ===========================================================================
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    -- launching and killing programs
    [ ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    , ((modMask,               xK_p     ), spawn "dmenu_run")
    , ((modMask .|. shiftMask, xK_c     ), kill)

    , ((modMask,               xK_space ), sendMessage NextLayout)
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    , ((modMask,               xK_n     ), refresh)

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  )
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp  )
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- locking and restarting
    , ((controlMask .|. shiftMask, xK_l), spawn "slock")
    , ((modMask, xK_r), spawn restartCmd)

    -- cycling screen focus
    , ((modMask .|. shiftMask, xK_h), onPrevNeighbour W.view)
    , ((modMask .|. shiftMask, xK_l), onNextNeighbour W.view)

    -- media keys
    , ((0, 0x1008FF11), spawn "pactl set-sink-volume 0 -5%")
    , ((0, 0x1008FF13), spawn "pactl set-sink-volume 0 +5%")
    , ((0, 0x1008FF12), spawn "pactl set-sink-mute 0 toggle")
    , ((0, 0x1008ff14), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    ]

    -- mod + N to select workspace N
    ++
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

--  ===========================================================================
--                                Mouse
--  ===========================================================================
myMouseBindings XConfig {XMonad.modMask = modMask} = M.fromList
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     \w -> focus w >> mouseMoveWindow w)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       \w -> focus w >> windows W.swapMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       \w -> focus w >> mouseResizeWindow w)
  ]

--  ===========================================================================
--                               Colors
--  ===========================================================================
myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#008000"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#7C7C7C",
    activeTextColor = "#CEFFAC",
    activeColor = "#000000",
    inactiveBorderColor = "#7C7C7C",
    inactiveTextColor = "#EEEEEE",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#FFB6B0"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#CEFFAC"

-- Width of the window border in pixels.
myBorderWidth = 1

--  ===========================================================================
--                                main
--  ===========================================================================
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad
    $ dynamicProjects projects
    $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = "   "
      }
      , manageHook = manageDocks <+> myManageHook
      , handleEventHook = docksEventHook
  }


defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders  myLayout,
    manageHook         = myManageHook
}