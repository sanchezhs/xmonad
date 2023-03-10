import System.IO
import System.Exit

import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig (additionalKeys, additionalMouseBindings)
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import qualified Codec.Binary.UTF8.String as UTF8

import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
---import XMonad.Layout.NoBorders
import XMonad.Layout.Fullscreen (fullscreenFull)
import XMonad.Layout.Cross(simpleCross)
import XMonad.Layout.Spiral(spiral)
import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens


import XMonad.Layout.CenteredMaster(centerMaster)

import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B
import Control.Monad (liftM2)
import qualified DBus as D
import qualified DBus.Client as D


myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

-- colours
normBord = "#4c566a"
focdBord = "#5e81ac"
fore     = "#DEE3E0"
back     = "#282c34"
winType  = "#c678dd"

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask = mod4Mask
encodeCChar = map fromIntegral . B.unpack
myFocusFollowsMouse = True
myBorderWidth = 2
myWorkspaces    = ["1","2","3","4","5","6","7","8","9","10"]


myBaseConfig = desktopConfig

-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
    ]
    where
    myCFloats = ["Arandr", "Arcolinux-calamares-tool.py", "Archlinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Thunar", "Archlinux-logout.py"]
    myTFloats = ["Downloads", "Save As..."]
    myRFloats = []
    myIgnores = ["desktop_window"]



myLayout = spacingRaw True (Border 0 5 5 5) True (Border 5 5 5 5) True $ avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tiled ||| Mirror tiled ||| spiral (6/7)  ||| ThreeColMid 1 (3/100) (1/2) ||| Full
    where
        tiled = Tall nmaster delta tiled_ratio
        nmaster = 1
        delta = 3/100
        tiled_ratio = 1/2


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList 

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), \w -> focus w >> windows W.shiftMaster)

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)

    ]


-- keys config

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList 
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [ ((modMask, xK_c), spawn "conky-toggle" )
  , ((modMask, xK_f), sendMessage $ Toggle NBFULL)
  , ((modMask, xK_q), kill )
  , ((modMask, xK_r), spawn "rofi-theme-selector" )
  , ((modMask, xK_v), spawn "pavucontrol" )
  , ((modMask, xK_x), spawn "archlinux-logout" )
  , ((modMask, xK_Return), spawn "alacritty" )
  , ((modMask, xK_d), spawn "thunar" )

  -- SUPER + SHIFT KEYS
  --, ((modMask .|. shiftMask , xK_d ), spawn "dmenu_run -i -nb '#191919' -nf '#fea63c' -sb '#fea63c' -sf '#191919' -fn 'NotoMonoRegular:bold:pixelsize=14'")
  , ((modMask .|. shiftMask, xK_d), spawn "rofi -show drun -show-icons -display-drun $")
  , ((modMask .|. shiftMask , xK_r ), spawn "xmonad --recompile && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)
  -- , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  -- CONTROL + ALT KEYS

  , ((controlMask .|. mod1Mask , xK_Next ), spawn "conky-rotate -n")
  , ((controlMask .|. mod1Mask , xK_Prior ), spawn "conky-rotate -p")
  , ((controlMask .|. mod1Mask , xK_a ), spawn "xfce4-appfinder")
  , ((controlMask .|. mod1Mask , xK_m ), spawn "xfce4-settings-manager")
  , ((controlMask .|. mod1Mask , xK_o ), spawn "$HOME/.xmonad/scripts/picom-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_r ), spawn "rofi-theme-selector")

  -- ALT + ... KEYS

  , ((mod1Mask, xK_r), spawn "xmonad --restart" )
  , ((mod1Mask, xK_F2), spawn "xfce4-appfinder --collapsed" )
  , ((mod1Mask, xK_F3), spawn "xfce4-appfinder" )


  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn "xfce4-taskmanager")

  --SCREENSHOTS

  , ((0, xK_Print), spawn "scrot '%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn "xfce4-screenshooter" )

  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn "amixer -q set Master toggle")

  -- Decrease volume
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 5%-")

  -- Increase volume
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 5%+")


--  , ((0, xF86XK_AudioPlay), spawn $ "mpc toggle")
--  , ((0, xF86XK_AudioNext), spawn $ "mpc next")
--  , ((0, xF86XK_AudioPrev), spawn $ "mpc prev")
--  , ((0, xF86XK_AudioStop), spawn $ "mpc stop")

  , ((0, xF86XK_AudioPlay), spawn "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --Focus selected desktop
  , ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
  --, ((modMask, xK_Tab), nextWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Left ), prevWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Right ), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

    -- Send window to workspace
  , ((modMask .|. shiftMask, xK_1), windows $ W.shift $ head myWorkspaces) 
  , ((modMask .|. shiftMask, xK_2), windows $ W.shift $ myWorkspaces !! 1)
  , ((modMask .|. shiftMask, xK_3), windows $ W.shift $ myWorkspaces !! 2)
  , ((modMask .|. shiftMask, xK_4), windows $ W.shift $ myWorkspaces !! 3)
  , ((modMask .|. shiftMask, xK_5), windows $ W.shift $ myWorkspaces !! 4)
  , ((modMask .|. shiftMask, xK_6), windows $ W.shift $ myWorkspaces !! 5)
  , ((modMask .|. shiftMask, xK_7), windows $ W.shift $ myWorkspaces !! 6)
  , ((modMask .|. shiftMask, xK_8), windows $ W.shift $ myWorkspaces !! 7)
  , ((modMask .|. shiftMask, xK_9), windows $ W.shift $ myWorkspaces !! 8)
  , ((modMask .|. shiftMask, xK_0), windows $ W.shift $ myWorkspaces !! 9)

    -- Switch workspace
  , ((modMask, xK_1), windows $ W.view $ head myWorkspaces)
  , ((modMask, xK_2), windows $ W.view $ myWorkspaces !! 1)
  , ((modMask, xK_3), windows $ W.view $ myWorkspaces !! 2)
  , ((modMask, xK_4), windows $ W.view $ myWorkspaces !! 3)
  , ((modMask, xK_5), windows $ W.view $ myWorkspaces !! 4)
  , ((modMask, xK_6), windows $ W.view $ myWorkspaces !! 5)
  , ((modMask, xK_7), windows $ W.view $ myWorkspaces !! 6)
  , ((modMask, xK_8), windows $ W.view $ myWorkspaces !! 7)
  , ((modMask, xK_9), windows $ W.view $ myWorkspaces !! 8)
  , ((modMask, xK_0), windows $ W.view $ myWorkspaces !! 9)


  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  --, ((modMask .|. shiftMask, xK_j), windows W.swapDown  )
  , ((modMask .|. shiftMask, xK_Left), windows W.swapDown  )

  -- Swap the focused window with the next window.
  --, ((controlMask .|. modMask, xK_Down), windows W.swapDown  )
  , ((modMask .|. shiftMask, xK_Right), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((controlMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. shiftMask , xK_l), sendMessage Expand)

  -- Push window back into tiling.
  , ((controlMask .|. shiftMask , xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]
-- ++
-- -- mod-[1..9], Switch to workspace N
-- -- mod-shift-[1..9], Move client to workspace N
-- [((m .|. modMask, k), windows $ f i)
-- --Keyboard layouts
-- --qwerty users use this line
--    | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7,xK_8,xK_9,xK_0]
--        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
--        , (\i -> W.view i . W.shift i, shiftMask)]]
 -- ++
 -- -- ctrl-shift-{w,e,r}, Move client to screen 1, 2, or 3
 -- -- [((m .|. controlMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
 -- --    | (key, sc) <- zip [xK_w, xK_e] [0..]
 -- --    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
--
 -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
 --     | (key, sc) <- zip [xK_Left, xK_Right] [0..]
 --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
 --     

main :: IO ()
main = do

    dbus <- D.connectSession
    -- Request access to the DBus name
    D.requestName dbus (D.busName_ "org.xmonad.Log")
        [D.nameAllowReplacement, D.nameReplaceExisting, D.nameDoNotQueue]


    xmonad . ewmh $
            myBaseConfig
                {startupHook = myStartupHook
, layoutHook = gaps [(U,35), (D,5), (R,5), (L,5)] $ myLayout ||| layoutHook myBaseConfig
, manageHook = manageSpawn <+> myManageHook <+> manageHook myBaseConfig
, modMask = myModMask
, borderWidth = myBorderWidth
, handleEventHook    = handleEventHook myBaseConfig
, focusFollowsMouse = myFocusFollowsMouse
, workspaces = myWorkspaces
, focusedBorderColor = focdBord
, normalBorderColor = normBord
, keys = myKeys
, mouseBindings = myMouseBindings
}
