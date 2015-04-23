{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module HW09 where

import Control.Monad
import System.Random
import Test.HUnit
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

-- Exercise 4: Quickcheck property that runs all 9 previous quickcheck properties
prop_ring :: (Ring a, Eq a) => a -> a -> a -> Property
prop_ring x y z = conjoin [prop_1 x y z, 
                           prop_2 x,
                           prop_3 x,
                           prop_4 x y,
                           prop_5 x y z,
                           prop_6 x,
                           prop_7 x y z,
                           prop_8 x y z,
                           prop_9 x]

-- Exercise 5: Find the broken ring in Ring.hs using quickCheck
testRings :: IO ()
testRings = do
    putStrLn "Testing Integer:"
    quickCheck (prop_ring :: Integer -> Integer -> Integer -> Property)
    putStrLn "Testing Mod5:"
    quickCheck (prop_ring :: Mod5 -> Mod5 -> Mod5 -> Property)
    putStrLn "Testing Mat2x2:"
    quickCheck (prop_ring :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Property)
    putStrLn "Testing Bool:"
    quickCheck (prop_ring :: Bool -> Bool -> Bool -> Property)

-- Exercise 6: Change BST functions to have an ordering constraint
data BST a = Leaf | Node (BST a) a (BST a)
  deriving Show

isBSTBetween :: (Ord a) => Maybe a -> Maybe a -> BST a -> Bool
isBSTBetween _ _ Leaf = True
isBSTBetween m_lower m_upper (Node left x right)
  = isBSTBetween m_lower  (Just x) left  &&
    isBSTBetween (Just x) m_upper  right &&
    case m_lower of
      Just lower -> lower <= x
      Nothing    -> True
    &&
    case m_upper of
      Just upper -> x <= upper
      Nothing    -> True

-- | Is this a valid BST?
isBST :: (Ord a) => BST a -> Bool
isBST = isBSTBetween Nothing Nothing

-- Exercise 7: Write an arbitrary instance for BST
-- Exercise 8: Improve randomness of tree generation
instance (Arbitrary a, Ord a, Random a) => Arbitrary (BST a) where
    arbitrary = do
        lower <- arbitrary 
        upper <- suchThat arbitrary (lower <)
        genBST lower upper 

genBST :: (Arbitrary a, Ord a, Random a) => a -> a -> Gen (BST a)
genBST lower upper = do
    x <- choose (lower, upper) 
    if x == lower then do
        rightTree <- genBST x upper
        return $ Node Leaf x rightTree
    else if x == upper then do
        leftTree <- genBST lower x
        return $ Node leftTree x Leaf
    else 
        frequency [
            (1, return Leaf), 
            (1, do 
                rightTree <- genBST x upper
                leftTree  <- genBST lower x
                return $ Node leftTree x rightTree)]

-- Exercise 9: Test parsers with HUnit
parserTests :: IO ()
parserTests = do
    result <- runTestTT $ TestList [
        "Integer: 2"     ~: parseAll "2"     ~?= Just (2  :: Integer),
        "Integer: 23"    ~: parseAll "23"    ~?= Just (23 :: Integer),
        "Integer: -4"    ~: parseAll "-4"    ~?= Just (-4 :: Integer),
        "Mod5: 4"        ~: parseAll "4"     ~?= Just (MkMod 4),
        "Mod5: 5"        ~: parseAll "5"     ~?= Just (MkMod 0),
        "Boolean: True"  ~: parseAll "True"  ~?= Just True,
        "Boolean: False" ~: parseAll "False" ~?= Just False,
        "Mat2x2: [0,0] [0,0]"      ~: parseAll "[[0,0][0,0]]"      ~?= Just (MkMat 0 0 0 0),
        "Mat2x2: [10,20] [30,-40]" ~: parseAll "[[10,20][30,-40]]" ~?= Just (MkMat 10 20 30 (-40))]
    print result

