{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-
Name: Steven Tomcavage
Collaborators: none 
Notes: 
-}

module HW05 where

import Data.List (stripPrefix)
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
    add (MkMat2x2 (a11, a12) (a21, a22)) (MkMat2x2 (b11, b12) (b21, b22)) = 
        MkMat2x2 (a11 + b11, a12 + b12) (a21 + b21, a22 + b22)
    mul (MkMat2x2 (a11, a12) (a21, a22)) (MkMat2x2 (b11, b12) (b21, b22)) =
        MkMat2x2 ((a11 * b11 + a12 * b21), (a11 * b12 + a12 * b22)) ((a21 * b11 + a22 * b21), (a21 * b12 + a22 * b22))

mat2x2InstanceWorks :: Bool
mat2x2InstanceWorks = add (MkMat2x2 (1, 2) (3, 4)) (MkMat2x2 (5, 6) (7, 8)) == MkMat2x2 (6, 8) (10, 12) &&
                      mul (MkMat2x2 (1, 2) (3, 4)) (MkMat2x2 (5, 6) (7, 8)) == MkMat2x2 (19, 22) (43, 50) &&
                      add (MkMat2x2 (1, 2) (3, 4)) addId == MkMat2x2 (1, 2) (3, 4) &&
                      mul (MkMat2x2 (1, 2) (3, 4)) mulId == MkMat2x2 (1, 2) (3, 4) &&
                      add (MkMat2x2 (1, 2) (3, 4)) (addInv $ MkMat2x2 (5, 6) (7, 8)) == MkMat2x2 (-4, -4) (-4, -4)

instance Parsable Mat2x2 where
    parse string = undefined

mat2x2ParsingWorks :: Bool
mat2x2ParsingWorks = undefined

-- Exercise 4: Create a Ring for Boolean arithmetic
instance Ring Bool where
    addId  = False 
    mulId  = True
    addInv = (not)
    add    = (/=)
    mul    = (&&)

booleanInstanceWorks :: Bool
booleanInstanceWorks = add True False  == True  &&
                       add True True   == False &&
                       add False False == False &&
                       mul True False  == False &&
                       mul True True   == True  &&
                       mul False False == False &&
                       add True  addId == True  &&
                       add False addId == False &&
                       mul True  mulId == True &&
                       mul False mulId == False

instance Parsable Bool where 
    parse string 
        | Just stringTail <- stripPrefix "True" string = Just (True, stringTail) 
        | Just stringTail <- stripPrefix "False" string = Just (False, stringTail) 
        | otherwise = Nothing

booleanParsingWorks :: Bool
booleanParsingWorks = (parse "True" == Just (True, "")) &&
                      (parse "False" == Just (False, "")) &&
                      (parseRing "True + False" == Just True) &&
                      (parseRing "True + True" == Just False) &&
                      (parseRing "True * False" == Just False)

