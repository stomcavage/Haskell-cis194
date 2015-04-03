{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module HW09 where

import Control.Monad
import Test.QuickCheck
import Ring

-- Exercise 1: Write arbitrary instances for Mod5 and Mat2x2
instance Arbitrary (Mod5) where
    arbitrary = genMod5 

genMod5 :: Gen Mod5
genMod5 = do
    x <- arbitrary 
    return $ mkMod x

instance Arbitrary (Mat2x2) where
    arbitrary = genMat2x2
    shrink    = shrinkMat2x2

genMat2x2 :: Gen Mat2x2
genMat2x2 = do
    [a, b, c, d] <- replicateM 4 arbitrary
    return $ MkMat a b c d

-- Exercise 2: Write shrink instance for Mat2x2
shrinkMat2x2 :: Mat2x2 -> [Mat2x2]
shrinkMat2x2 (MkMat a b c d) = 
    [MkMat w x y z | w <- shrinkIntegral a, x <- shrinkIntegral b, 
                     y <- shrinkIntegral c, z <- shrinkIntegral d] 

-- Exercise 3: Write quickcheck properties for Rings
prop_1 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_1 x y z = add (add x y) z == add x (add y z)

prop_2 :: (Ring a, Eq a) => a -> Bool
prop_2 x = add x addId == x && add addId x == x

prop_3 :: (Ring a, Eq a) => a -> Bool
prop_3 x = add x (addInv x) == add (addInv x) x 

prop_4 :: (Ring a, Eq a) => a -> a -> Bool
prop_4 x y = add x y == add y x 

prop_5 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_5 x y z = mul (mul x y) z == mul x (mul y z)

prop_6 :: (Ring a, Eq a) => a -> Bool
prop_6 x = mul x mulId == x && mul mulId x == x

prop_7 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_7 x y z = mul x (add y z) == add (mul x y) (mul x z)

prop_8 :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_8 x y z = mul (add y z) x == add (mul y x) (mul z x)

prop_9 :: (Ring a, Eq a) => a -> Bool
prop_9 x = add x (addInv x) == addId && add (addInv x) x == addId

