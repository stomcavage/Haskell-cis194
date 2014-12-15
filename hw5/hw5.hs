{-# OPTIONS_GHC -Wall #-}
{-
Name: Steven Tomcavage
Collaborators: none 
Notes: 
-}

module HW05 where

import Data.Maybe
import Ring
import Parser

-- Exercise 2: Create Ring and Parsable instances for Mod5
data Mod5 = MkMod Integer
    deriving (Show, Eq)

instance Ring (Mod5) where
    addId = MkMod 0
    mulId = MkMod 1
    addInv (MkMod a) = MkMod $ negate a
    add (MkMod a) (MkMod b) = MkMod $ (a + b) `mod` 5
    mul (MkMod a) (MkMod b) = MkMod $ (a * b) `mod` 5

mod5InstanceWorks :: Bool
mod5InstanceWorks = add (MkMod 3) (MkMod 3) == MkMod 1 &&
                    add (MkMod 4) addId     == MkMod 4 &&
                    mul (MkMod 3) (MkMod 3) == MkMod 4 &&
                    mul (MkMod 4) mulId     == MkMod 4 &&
                    add (MkMod 4) (addInv (MkMod 3)) == MkMod 1

instance Parsable Mod5 where
    parse string = case (listToMaybe $ reads string) of
                       Nothing    -> Nothing
                       Just (x,y) -> Just (MkMod x, y)

mod5ParsingWorks :: Bool
mod5ParsingWorks = (parse "3" == Just ((MkMod 3), "")) &&
                   (parseRing "1 + 2 * 5" == Just (MkMod 1)) &&
                   (addId == (MkMod 0))

-- Exercise 3: Create Ring and Parsable instances for a 2x2 matrix
data Mat2x2 = MkMat2x2 (Integer, Integer) (Integer, Integer)
    deriving (Show, Eq)

instance Ring (Mat2x2) where
    addId = MkMat2x2 (0, 0) (0, 0)
    mulId = MkMat2x2 (1, 0) (0, 1)
    addInv (MkMat2x2 (a, b) (c, d)) = MkMat2x2 (-a, -b) (-c, -d)
    add (MkMat2x2 (a1, b1) (c1, d1)) (MkMat2x2 (a2, b2) (c2, d2)) = MkMat2x2 (a1 + a2, b1 + b2) (c1 + c2, d1 + d2)
    mul (MkMat2x2 (a1, b1) (c1, d1)) (MkMat2x2 (a2, b2) (c2, d2)) = 
        MkMat2x2 ((a1 * a2 + b1 * c2), (a1 * b2 + b1 * d2)) ((c1 * a2 + d1 * c2), (c1 * b2 + d1 * d2))

