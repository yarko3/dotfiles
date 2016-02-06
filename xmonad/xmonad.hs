import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import System.IO (hPutStrLn)

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig {
            terminal = myTerminal,
            modMask = myModMask,
            manageHook = manageDocks <+> manageHook defaultConfig,
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

myTerminal = "gnome-terminal"
myModMask = mod4Mask
