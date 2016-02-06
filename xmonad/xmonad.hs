import System.IO (hPutStrLn)
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

myTerminal = "gnome-terminal"
myModMask = mod4Mask

-- Program names that should not be managed and tiled
myManageHook = composeAll [
        className =? "Gimp" --> doFloat
    ]

main = do
    xmproc <- spawnPipe "xmobar"
    _ <- spawn myTerminal
    xmonad $ defaultConfig {
            terminal = myTerminal,
            modMask = myModMask,
            manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig,
            layoutHook = avoidStruts $ layoutHook defaultConfig,
            logHook = dynamicLogWithPP xmobarPP {
                            ppOutput = hPutStrLn xmproc,
                            ppTitle = xmobarColor "green" "" . shorten 50
                        }
        } `additionalKeys`
        [
            ((mod4Mask, xK_Return), spawn myTerminal),
            ((controlMask .|. shiftMask, xK_l), spawn "slock")
        ]

