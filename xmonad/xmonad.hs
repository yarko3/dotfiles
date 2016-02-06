import XMonad
import XMonad.Hooks.DynamicLog

main = do
    conf <- dzen defaultConfig
    xmonad $ conf
        {
            terminal = "gnome-terminal",
            modMask = mod4Mask
        }
