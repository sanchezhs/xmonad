module Config.Configs where

import XMonad
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W


------------------------------------------------------------------------------------
--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myTerminal :: [Char]
myTerminal = "alacritty"

normBord :: String
normBord = "#4c566a"

focdBord :: String
focdBord = "#E5B8F4"

myModMask :: KeyMask
myModMask = mod4Mask

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth :: Dimension
myBorderWidth = 2

myWorkspaces :: [WorkspaceId]
myWorkspaces = map show [1 .. 9 :: Int]

------------------------------------------------------------------------------------

layouts :: [(String, Int)]
layouts = zip 
  ["Spacing Tall", "Spacing Mirror Tall", "Spacing Spiral",  "Spacing ThreeCol", "Spacing Full", "Tall", "Mirror Tall", "Full"]
  [0 ..]


scratchpads = [
    NS "alacritty" "alacritty" (className =? "Alacritty")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

    NS "thunar" "thunar" (className =? "Thunar")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) 
  ] 
