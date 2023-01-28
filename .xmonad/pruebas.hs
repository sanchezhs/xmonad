{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Brackes where


import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Operations as O
import XMonad.Layout.IndependentScreens
import Data.List
import qualified Data.Map as M
import Data.Maybe

w :: [String]
w=["0_1","0_2","0_3","0_4","0_5","0_6","0_7","0_8","0_9","1_1","1_2","1_3","1_4","1_5","1_6","1_7","1_8","1_9"]


myWorkspaces' :: [String]
myWorkspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

ws :: [PhysicalWorkspace]
ws = withScreens 2 myWorkspaces'

myWorkspaceIndices :: M.Map String Integer
myWorkspaceIndices = M.fromList $ zip myWorkspaces' [1..] -- (,) == \x y -> (x,y)

clickable :: [Char] -> [Char]
clickable ws = "<action=xdotool key super+"++show i++">"++ws++"</action>"
    where i = fromJust $ M.lookup ws myWorkspaceIndices

-- do  windows $ onCurrentScreen W.view $ snd $ unmarshall  ws
clickable' :: [Char] -> [Char]
clickable' ws = "<action=xdotool key super+"++show i++">"++ws'++"</action>"
    where
        (_,ws') = unmarshall ws
        i       = fromJust $ M.lookup (snd $ unmarshall ws) myWorkspaceIndices

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset


countWindows :: ScreenId  -> X (Maybe String)
countWindows (S s) = do
    ws <- gets $ W.screens . windowset
    if gets W.screen w' == s
        then gets $ Just . show . length . W.integrate' . W.stack . W.workspace . w'
        else gets $ Just . show . length . W.integrate' . W.stack . W.workspace . w''
    where
        w' = head ws
        w'' = ws !! 1

countWindows' :: ScreenId -> X (Maybe String)
countWindows' (S s) = do
    ws <- gets $ W.screens . windowset
    case filter (\x -> W.screen x == S s) ws of
        (w:_) -> return $ Just $ show $ length $ W.integrate' $ W.stack $ W.workspace w
        [] -> return Nothing

