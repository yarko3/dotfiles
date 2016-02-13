import System.IO (hPutStrLn)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.IndependentScreens
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

myTerminal = "gnome-terminal"
myModMask = mod4Mask

-- Program names that should not be managed and tiled
composeHook = composeAll [
        className =? "Gimp" --> doFloat
    ]

myManageHook = manageDocks <+> composeHook <+> manageHook defaultConfig
myLayoutHook = avoidStruts $ layoutHook defaultConfig
myLogHook xmproc = dynamicLogWithPP xmobarPP {
                            ppOutput = hPutStrLn xmproc,
                            ppTitle = xmobarColor "green" "" . shorten 50
                        }

restartCmd :: String
restartCmd = "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi"

-- Independent Screens
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..6 :: Int]

-- Where my six monitors happen to be in Xconfig
xOrder = [2, 1, 5,
          3, 0, 4]

main = do
    xmproc <- spawnPipe "xmobar"
    _ <- spawn myTerminal
    xmonad $ defaultConfig
        { terminal = myTerminal
        , modMask = myModMask
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
        -- mod-{q,w,e,a,s,d} %! Switch focus to physical/Xinerama screens
        -- mod-shift-{q,w,e,a,s,d} %! Throw client to physical/Xinerama screen
        [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
          | (key, sc) <- zip [xK_q, xK_w, xK_e, xK_a, xK_s, xK_d, xK_f] xOrder
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
        ]

        ++
        -- mod-[1..6] %! Switch focus to workspace N (TODO: of this screen)
        -- mod-shift-[1..6] %! Move client to workspace N (TODO: of this screen)
        [ ((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
        ]
        )
