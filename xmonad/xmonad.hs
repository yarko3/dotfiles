import System.IO (hPutStrLn, Handle)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

import Control.Applicative
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Display (closeDisplay)

myTerminal = "gnome-terminal"
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

-- Get all Xinerama Screen #'s
getScreens :: IO [Int]
getScreens = openDisplay "" >>= liftA2 (<*) f closeDisplay
    where f = fmap (zipWith const [0..]) . getScreenInfo

-- Relationship between physical placement of monitor and screen number in Xinerama
xOrder = [2, 1, 5,
          3, 0, 4]
xKeys = [xK_q, xK_w, xK_e,
         xK_a, xK_s, xK_d]

-- xmobar on every screen
xmobarScreen :: Int -> IO Handle
xmobarScreen = spawnPipe . ("xmobar -x " ++) . show

conf myWorkspaces xmproc = defaultConfig
        { terminal = myTerminal
        , modMask = myModMask
        , borderWidth = 2
        , XMonad.workspaces = myWorkspaces
        , manageHook = myManageHook
        , layoutHook = myLayoutHook
        , logHook = myLogHook xmproc
        } `additionalKeys` (
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
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
        ]
        )
--------------------------------------------------------------------------------
main = do
    xmproc <- spawnPipe "xmobar"
    screenCt <- countScreens
    _ <- spawn myTerminal
    let myWorkspaces = withScreens screenCt $ map show [1..6]
    xmonad $ conf myWorkspaces xmproc
