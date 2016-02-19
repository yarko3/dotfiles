import Control.Applicative
import System.Exit
import System.IO (hPutStrLn, Handle)
import XMonad
import XMonad.Core (WorkspaceId)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

-- Put any local configuration here
import XMonadLocal

main = do
    screenCt <- countScreens
    xmproc <- spawnPipe "xmobar"
    _ <- spawn myTerminal
    xmonad $ conf screenCt xmproc

myTerminal :: String
myTerminal = "gnome-terminal"

-- Program names that should not be managed and tiled
composeHook = composeAll [
        className =? "1-BLOOMBERG" --> doFloat,
        className =? "2-BLOOMBERG" --> doFloat,
        className =? "3-BLOOMBERG" --> doFloat,
        className =? "4-BLOOMBERG" --> doFloat,
        className =? "Bloomberg" --> doFloat,
        className =? "Gimp" --> doFloat,
        className =? "IB" --> doFloat,
        className =? "Laundhpad - Ben View" --> doFloat
    ]

myManageHook = manageDocks <+> composeHook <+> manageHook defaultConfig
myLayoutHook = avoidStruts $ layoutHook defaultConfig
myLogHook xmproc = dynamicLogWithPP xmobarPP {
                            ppOutput = hPutStrLn xmproc,
                            ppTitle = xmobarColor "green" "" . shorten 50
                        }

restartCmd :: String
restartCmd = "if type xmonad; then xmonad --recompile && \
              \xmonad --restart; else xmessage xmonad not in PATH; fi"

conf screenCt xmproc =
        let myWorkspaces = withScreens screenCt $ map show [1..9] in
        defaultConfig
        { terminal = myTerminal
        , modMask = mod4Mask
        , borderWidth = 3
        , focusFollowsMouse = False
        , XMonad.workspaces = myWorkspaces
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        , logHook = myLogHook xmproc
        , keys = myKeys
        }

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
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

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

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
    ]
    ++
    -- mod-{q,w,e,a,s,d} %! Switch focus to physical/Xinerama screens
    -- mod-shift-{q,w,e,a,s,d} %! Throw client to physical/Xinerama screen
    [ ((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip xKeys xOrder
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
    ++
    -- mod-[1..6] %! Switch focus to workspace N of this screen
    -- mod-shift-[1..6] %! Move client to workspace N of this screen
    [ ((m .|. modMask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
