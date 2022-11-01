import Control.Applicative
import Data.Default
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

myWorkspaces = [wsWORK_TERM, wsWORK_BROWSER, wsMEDIA, wsSCRATCH]

projects :: [Project]
projects =
    [ Project   { projectName      = wsWORK_TERM
                , projectDirectory = "~/"
                , projectStartHook = Just $ spawn myTerminal
            }

    , Project   { projectName       = wsWORK_BROWSER
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsWORK_BROWSER "google-chrome --new-window gmail.com"
                                                spawnOn wsWORK_BROWSER "google-chrome --new-window calendar.google.com"
            }

    , Project   { projectName       = wsMEDIA
                , projectDirectory  = "~/"
                , projectStartHook  = Just $ do spawnOn wsMEDIA "google-chrome --new-window news.ycombinator.com spectrum.ieee.org mail.google.com/mail/u/1/#inbox messages.google.com messenger.com"
                                                spawnOn wsMEDIA "spotify"
            }

    , Project   { projectName       = wsSCRATCH
                , projectDirectory  = "~/"
                , projectStartHook  = Nothing
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
    , ((modMask,                    xK_p      ), spawn "dmenu_run")
    , ((modMask .|. shiftMask,      xK_c      ), kill)

    , ((modMask,                    xK_space  ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,      xK_space  ), setLayout $ XMonad.layoutHook conf)

    , ((modMask,                    xK_n           ), refresh)

    -- move focus up or down the window stack
    , ((modMask,                    xK_Tab    ), windows W.focusDown)
    , ((modMask .|. shiftMask,      xK_Tab    ), windows W.focusUp  )
    , ((modMask,                    xK_j      ), windows W.focusDown)
    , ((modMask,                    xK_k      ), windows W.focusUp  )
    , ((modMask,                    xK_m      ), windows W.focusMaster)

    -- floating layer support
    , ((modMask,                    xK_t      ), withFocused $ windows . W.sink)

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask,                    xK_comma  ), sendMessage (IncMasterN 1))
    , ((modMask,                    xK_period ), sendMessage (IncMasterN (-1)))

    -- locking, quitting, restarting
    , ((controlMask .|. shiftMask,  xK_l      ), spawn "slock")
    , ((modMask .|. shiftMask,      xK_q      ), io exitSuccess)
    , ((modMask,                    xK_r      ), spawn restartCmd)

    -- cycling screen focus
    , ((modMask,                    xK_h      ), onPrevNeighbour def W.view)
    , ((modMask,                    xK_l      ), onNextNeighbour def W.view)

    -- media keys
    , ((0, 0x1008FF11), spawn "amixer set Master 5%-")
    , ((0, 0x1008FF13), spawn "amixer set Master 5%+")
    , ((0, 0x1008FF12), spawn "amixer set Master toggle")
    , ((0, 0x1008FF14), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.PlayPause")
    , ((0, 0x1008FF16), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Previous")
    , ((0, 0x1008FF17), spawn "dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.mpris.MediaPlayer2.Player.Next")
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
--                                Theme
--  ===========================================================================
myFocusFollowsMouse = False
myClickJustFocuses = True

myNormalBorderColor  = "#7c7c7c"
myFocusedBorderColor = "#008000"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = def {
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
myBorderWidth = 4

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


defaults = def {
    -- simple stuff
    terminal           = myTerminal,
    borderWidth        = myBorderWidth,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,
    clickJustFocuses   = myClickJustFocuses,
    focusFollowsMouse  = myFocusFollowsMouse,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders  myLayout,
    manageHook         = myManageHook
}
