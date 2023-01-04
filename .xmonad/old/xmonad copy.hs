--import System.IO
--import System.Exit

import XMonad
import qualified XMonad.Operations as O -- focus temp
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
--import XMonad.Hooks.DynamicIcons
import XMonad.Config.Desktop
--import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe, hPutStrLn)
import XMonad.Util.SpawnOnce(spawnOnce)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings, additionalKeysP)
import XMonad.Actions.CycleWS
--import XMonad.Hooks.UrgencyHook
--import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
--import XMonad.Layout.ResizableTile
---import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens


--import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
--import qualified Data.ByteString as B
--import Control.Monad as C

--import qualified XMonad.Layout.Renamed as R
import XMonad.Actions.UpdatePointer
--import qualified XMonad.Actions.PhysicalScreens as P
--import Data.Default
--import XMonad.Util.NamedScratchpad
import Data.Maybe (fromJust, isJust, fromMaybe, isNothing)
--import Data.Monoid
import Color.Nord
--import Data.List
--import Graphics.X11.Types(Window)
import XMonad.Util.NamedScratchpad

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

-- Default xmonad config for windows, panels...
myBaseConfig = desktopConfig

-- Actions to perform on startup
myStartupHook :: X ()
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    spawn "killall trayer"
    --spawn ("sleep 2 && trayer --edge top --align right --widthtype request --padding 8 --SetDockType true --SetPartialStrut true --expand true --monitor 1 --transparent true --alpha 0 " ++ colorTrayer ++ " --height 22")
    spawn "sleep 2 && polybar-msg action layout1 hook 0 && polybar-msg action layout2 hook 0"
    setWMName "LG3D"


layouts :: [(String, Int)]
layouts = zip 
  ["Spacing Tall", "Spacing Mirror Tall", "Spacing Spiral", 
  "Spacing ThreeCol", "Spacing Full", "Tall", "Mirror Tall", "Full"]
  [1 ..]


scratchpads = [
-- run htop in xterm, find it by title, use default floating window placement
    NS "htop" "alacritty -e htop" (title =? "htop") defaultFloating ,

-- run stardict, find it by class name, place it in the floating window
-- 1/6 of screen width from the left, 1/6 of screen height
-- from the top, 2/3 of screen width by 2/3 of screen height
    NS "thunar" "thunar" (className =? "Thunar")
        (customFloating $ W.RationalRect (1/6) (1/6) (2/3) (2/3)) ,

-- run gvim, find by role, don't float
    NS "notes" "gvim --role notes ~/notes.txt" (role =? "notes") nonFloating
  ] where role = stringProperty "WM_WINDOW_ROLE"



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

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1..] -- (,) == \x y -> (x,y)

clickable :: WorkspaceId -> [Char]
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

clickable' :: [Char] -> [Char] -> [Char]
clickable' icon ws = addActions [ (show i, 1), ("q", 2), ("Left", 4), ("Right", 5) ] icon
                    where i = fromJust $ M.lookup ws myWorkspaceIndices

addActions :: [(String, Int)] -> String -> String
addActions [] ws = ws
addActions (x:xs) ws = addActions xs (actionPrefix ++ k ++ actionButton ++ show b ++ ">" ++ ws ++ actionSuffix)
    where k = fst x
          b = snd x

actionPrefix, actionButton, actionSuffix :: [Char]
actionPrefix = "<action=`xdotool key super+"
actionButton = "` button="
actionSuffix = "</action>"

getFocusedWindow :: X (Maybe Window)
getFocusedWindow = gets $ W.peek . windowset


-- Switch focus between monitors
focusAnotherScreen :: X ()
focusAnotherScreen = do
  w <- gets (aux . W.stack . W.workspace . head . W.visible . windowset)
  O.focus w

aux :: Maybe (W.Stack Window) -> Window
aux (Just (W.Stack f u d)) = f
aux Nothing                = 0


currentScreen :: X ScreenId
currentScreen = gets (W.screen . W.current . windowset)

getCurrentScreen :: ScreenId -> Int 
getCurrentScreen (S i) = i

getHook :: String -> [(String,Int)] -> String
getHook _ [] = "9"
getHook x ((a,b):xs) = if x == a then show b else getHook x xs


getLayout ::  X ()
getLayout  = do
  sendMessage NextLayout
  layout <- gets $ description . W.layout . W.workspace . W.current . windowset
  sc     <- currentScreen
  spawn $ "polybar-msg action layout" ++ show (getCurrentScreen sc) ++ " hook "  ++ getHook layout layouts
  


toggleFloat :: Window -> X ()
toggleFloat w =
  windows (\s ->
        if M.member w (W.floating s)
          then W.sink w s
          else W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s )

------------------------------------------------------------------------------------

{-move :: Int -> X ()
move  s = do
  curr <- currentScreen
  focus <- getFocusedWindow
  if focus == Nothing then return () else windows $ W.shift =<< W.tag . W.workspace . head . W.visible


moveFocusedWindowToAnotherScreen :: X ()
moveFocusedWindowToAnotherScreen = do
  focused <- getFocusedWindow
  ws <- gets windowset
  let screen = W.current ws
  let newScreen = if screen == 0 then 1 else 0
  windows $ W.shift newScreen --(fromMaybe 0 focused)
-}

------------------------------------------------------------------------------------

-- The action to run when a new window is opened
myManageHook :: ManageHook
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    ]
    where
    myCFloats = ["Nitrogen", "Arandr", "Arcolinux-calamares-tool.py", "Archlinux-tweak-tool.py",
                "Arcolinux-welcome-app.py", "Thunar", "Archlinux-logout.py", "Pavucontrol"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]


------------------------------------------------------------------------------------


myLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6/7)  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2

------------------------------------------------------------------------------------

myMouseBindings :: XConfig l -> M.Map (KeyMask, Button) (Window -> X ())
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    ]


-- keys config
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS
  [ ((modMask, xK_q), kill )
  , ((modMask, xK_v), spawn "pavucontrol" )
  , ((modMask, xK_x), spawn "archlinux-logout" )
  , ((modMask, xK_Return), spawn "alacritty" )
  , ((modMask, xK_z), spawn "thunar" )
  , ((modMask, xK_b), spawn "firefox" )

  , ((mod1Mask, xK_t), namedScratchpadAction scratchpads "thunar")

  -- SUPER + SHIFT KEYS
  --, ((modMask .|. shiftMask , xK_d ), spawn "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
  , ((modMask .|. shiftMask, xK_z), spawn "rofi -show drun -show-icons -display-drun $")
  , ((modMask .|. shiftMask , xK_r ), spawn "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)

  --, ((modMask .|. shiftMask , xK_w ), getLayout')

  --MULTIMEDIA KEYS

  , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")
  , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn "playerctl stop")

  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Send to right screen, then focus
  --, ((modMask .|. shiftMask, xK_d), P.sendToScreen def 1 >> P.viewScreen def 1)
  -- Send to left screen, then focus
  --, ((modMask .|. shiftMask, xK_a), P.sendToScreen def 0 >> P.viewScreen def 0)
  , ((modMask .|. shiftMask, xK_d), windows $ W.shift =<< W.tag . W.workspace . head . W.visible)

  -- Toggle floating.
  , ((modMask .|. shiftMask , xK_f), withFocused toggleFloat)

  -- Full screen
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  -- Shifts the window with the tag of the first visible workspace to the current workspace.
  , ((modMask .|. shiftMask, xK_s), windows $ W.greedyView =<< W.tag . W.workspace . head . W.visible)

  -- Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Focus
  -- Focus selected desktop
  , ((mod1Mask, xK_Tab), nextWS)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)
  , ((modMask, xK_k), windows W.focusUp)

  -- Move focus to the previous window.
  --, ((modMask, xK_s), windows W.focusUp  )
  , ((modMask, xK_Down), windows W.focusDown)
  , ((modMask, xK_Up), windows W.focusUp)

  -- Move focus to the master window.
  , ((modMask, xK_m), windows W.focusMaster)
  -- Move focus to second screen.
  , ((modMask, xK_Right), focusAnotherScreen )
  , ((modMask, xK_Left), focusAnotherScreen )


  -- Swaps
  -- Swap focused with the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp  )



  -- Shrink the master area.
  , ((modMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((modMask .|. shiftMask , xK_l), sendMessage Expand)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]

   ++
 -- mod-[1..9],       Switch to workspace N
 -- mod-shift-[1..9], Move client to workspace N
-- [((m .|. modMask, k), windows $ f i)
--    | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
--        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)
--        , (\i -> W.view i . W.shift i, shiftMask)]]

    [((m .|. modMask, k), windows $ onCurrentScreen f i)
        | (i, k) <- zip (workspaces' conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------------------

main :: IO ()
main = do
--  xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.xmonad/xmobarrc.hs"
--  xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.xmonad/xmobarrc_left.hs"

  xmonad . ewmh $ desktopConfig {

        startupHook = myStartupHook
      , layoutHook = gaps [(U,35), (D,5), (R,5), (L,5)] $ myLayout ||| layoutHook desktopConfig
      , manageHook = manageSpawn <+> myManageHook <+> manageHook desktopConfig <+> namedScratchpadManageHook scratchpads
      , modMask = myModMask
      , borderWidth = myBorderWidth
      , terminal = myTerminal
      , handleEventHook    = handleEventHook desktopConfig
      , focusFollowsMouse = myFocusFollowsMouse
      , workspaces = withScreens 2 myWorkspaces
      , focusedBorderColor = focdBord
      , normalBorderColor = normBord
      , keys = myKeys
      , mouseBindings = myMouseBindings
--      , logHook =  dynamicLogWithPP  xmobarPP
--          { ppOutput = \x -> hPutStrLn xmproc0 x   -- xmobar on monitor 1
--                          >> hPutStrLn xmproc1 x   -- xmobar on monitor 2
--          , ppCurrent = xmobarColor colorFore "" .  clickable' wsIconFull
--            -- Visible but not current workspace
--          , ppVisible = xmobarColor color14 "" . clickable' wsIconFull
--            -- Hidden workspace
--          , ppHidden = xmobarColor color07 "" . clickable' wsIconHidden
--            -- Hidden workspaces (no windows)
--          , ppHiddenNoWindows = xmobarColor color09 ""  . clickable' wsIconEmpty
--            -- Title of active window
--          , ppTitle = xmobarColor color16 "" . shorten 60
--            -- Separator character
--          , ppSep =  "<fc=" ++ color09 ++ "> <fn=1>|</fn> </fc>"
--
--          , ppWsSep = ""
--            -- Urgent workspace
--          , ppUrgent = xmobarColor color02 "" . wrap "!" "!"
--            -- Adding # of windows on current workspace to the bar
--          , ppExtras  = [windowCount]
--            -- order of things in xmobar
--          , ppOrder  = \(ws:l:t:ex) -> [ws,l]++ex++[t]
--          } >> updatePointer (0.5, 0.5) (0, 0)

}
  where
    blue     = xmobarColor "#B48EAD" ""
    wsIconFull   = "  <fn=2>\xf111</fn>   "
    wsIconHidden = "  <fn=2>\xf111</fn>   "
    wsIconEmpty  = "  <fn=2>\xf10c</fn>   "