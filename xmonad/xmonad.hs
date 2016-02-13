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

-- Disable greedyView
isVisible w ws = any ((w ==) . W.tag . W.workspace) (W.visible ws)
lazyView w ws = if isVisible w ws then ws else W.view w ws

-- Independent Screens
myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1..6 :: Int]

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

        -- mod-{q,w,e,a,s,d} %! Switch to physical/Xinerama screens [1..6]
        -- mod-shift-{q,w,e,a,s,d} %! Move client to screen [1..6]
        [((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_q, xK_w, xK_e, xK_a, xK_s, xK_d, xK_f] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


        {--

        -- mod-[1..6] %! Switch to workspace N
        -- mod-shift-[1..6] %! Move client to workspace N
        [((m .|. mod4Mask, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++
        ++
         --
         -- mod-[1..9], Switch to workspace N
         -- mod-shift-[1..9], Move client to workspace N
         --
        [((m .|. mod4Mask, k), windows $ f i)
        , (i, k) <- zip (myWorkspaces) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-}
        )
