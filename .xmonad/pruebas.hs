{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Brackes where
import Data.List

data Pose = P Float Float Float deriving Show

p0, u1 :: Pose
p0 = P 0 0 0

u1 = P 0 1 0
u2 = P 1 1 (pi/2)
u3 = P 2 0 0

comp :: Pose -> Pose -> Pose
comp (P x y a) (P x' y' a') = P x'' y'' a''
    where
        x'' = x + x' * cos a - y' * sin a
        y'' = y + x' * sin a + y' * cos a
        a'' = a + a'

compSeq :: Pose -> [Pose] -> Pose 
compSeq p0 (p:ps) = undefined 

getSecondNumbers :: [String] -> [String]
getSecondNumbers = map (last . words . map (\c -> if c == '_' then ' ' else c))

w :: [String]
w=["0_1","0_2","0_3","0_4","0_5","0_6","0_7","0_8","0_9","1_1","1_2","1_3","1_4","1_5","1_6","1_7","1_8","1_9"]