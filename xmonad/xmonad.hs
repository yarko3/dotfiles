import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.EZConfig (additionalKeys)

main = do
    conf <- dzen defaultConfig
    xmonad $ conf
        {
            terminal = myTerminal,
            modMask = myModMask
        } `additionalKeys`
        [
            ((mod4Mask, xK_Return), spawn myTerminal),
            ((controlMask .|. shiftMask, xK_l), spawn "slock")
        ]

myTerminal = "gnome-terminal"
myModMask = mod4Mask
