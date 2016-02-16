import Control.Applicative
import System.Exit
import System.IO (hPutStrLn, Handle)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import XMonad.Util.Run (spawnPipe)
import qualified Data.Map as M
import qualified XMonad.StackSet as W

main = do
    screenCt <- countScreens
    xmproc <- spawnPipe "xmobar"
    _ <- spawn myTerminal
    xmonad $ conf xmproc

myTerminal :: String
myTerminal = "gnome-terminal"

myModMask :: KeyMask
myModMask = mod4Mask

-- Program names that should not be managed and tiled
composeHook = composeAll [
        className =? "Bloomberg" --> doFloat,
        className =? "Gimp" --> doFloat
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

-- Relationship between physical placement of monitor and screen number in Xinerama
xOrder :: [ScreenId]
xOrder = [2, 1, 5,
          3, 0, 4]

xKeys :: [KeySym]
xKeys = [xK_q, xK_w, xK_e,
         xK_a, xK_s, xK_d]

conf xmproc =
        let myWorkspaces = withScreens 6 $ map show [1..9] in
        defaultConfig
        { terminal = myTerminal
        , modMask = myModMask
        , borderWidth = 3
        , XMonad.workspaces = myWorkspaces
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        , logHook = myLogHook xmproc
        , keys = myKeys
        }


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
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
    , ((modMask,               xK_m     ), windows W.focusMaster  )

    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster)
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
    ]
    ++
    [ ((mod4Mask, xK_Return), spawn myTerminal)
    , ((controlMask .|. shiftMask, xK_l), spawn "slock")
    , ((mod4Mask, xK_r), spawn restartCmd)
    ]

    ++
    -- mod-{w,e,r,s,d,f} %! Switch focus to physical/Xinerama screens
    -- mod-shift-{w,e,r,s,d,f} %! Throw client to physical/Xinerama screen
    [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip xKeys xOrder
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]

    ++
    -- mod-[1..6] %! Switch focus to workspace N of this screen
    -- mod-shift-[1..6] %! Move client to workspace N of this screen
    [ ((m .|. mod4Mask, k), windows $ onCurrentScreen f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
