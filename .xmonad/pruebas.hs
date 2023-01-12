{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Brackes where
import Data.List


l = [("Spacing Tall",0),("Spacing Mirror Tall",1),("Spacing Spiral",2),("Spacing ThreeCol",3),("Spacing Full",4),("Tall",5),("Mirror Tall",6),("Full",7)]


getHook :: String -> [(String, Int)] -> String
getHook l xs = (\ [(a,b)] -> show b) $ filter (\ (a,b) -> a == l) xs
   