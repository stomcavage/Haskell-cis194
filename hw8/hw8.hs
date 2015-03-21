module HW08 where

import Control.Monad.Random
import Data.List 
import Data.Maybe
import Data.Monoid
import Text.Read (readMaybe)

-- Exercise 1: Validate string according to format
stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
    where go :: String -> Maybe String
          go []     = return "valid"
          go (x:xs) = do
            times <- readMaybe [x] :: Maybe Int
            rest  <- stripPrefix (replicate times 'a') xs
            go rest

testStringFitsFormat :: Bool
testStringFitsFormat = stringFitsFormat "3aaa2aa" 
                       && stringFitsFormat "9aaaaaaaaa"   
                       && stringFitsFormat "0"             
                       && stringFitsFormat "001a"           
                       && stringFitsFormat "2aa2aa"          
                       && not (stringFitsFormat "3aaa2a")     
                       && not (stringFitsFormat "10aaaaaaaaaa")
                       && not (stringFitsFormat "1")          
                       && not (stringFitsFormat "100a")        
                       && not (stringFitsFormat "2bb2bb")

-- Exercise 2: Modified fizz-buzz
specialNumbers :: [Int]
specialNumbers = [ x | x <- [1..100], x `mod` 5 == 0 && x `mod` 7 /= 0 ]

testSpecialNumbers :: Bool
testSpecialNumbers = specialNumbers == 
    [5,10,15,20,25,30,40,45,50,55,60,65,75,80,85,90,95,100]

-- Include some types for playing Risk
type StdRand = Rand StdGen
type Army    = Int
type DieRoll = Int

data ArmyCounts = ArmyCounts { attackers :: Army, defenders :: Army }
    deriving Show

-- Exercise 3: Random die roll function
dieRoll :: StdRand DieRoll
dieRoll = getRandomR (1, 6)

-- Exercise 4: Determine army counts after rolling dice in Risk
instance Monoid ArmyCounts where
    mempty = ArmyCounts { 
        attackers = 0, 
        defenders = 0 }
    mappend ac1 ac2 = ArmyCounts { 
        attackers = attackers ac1 - attackers ac2, 
        defenders = defenders ac1 - defenders ac2 }

battleResults :: [DieRoll] -> [DieRoll] -> ArmyCounts
battleResults attack defend = foldl mappend mempty $ zipWith roll sortedAttack sortedDefend
    where roll a d | a > d  = ArmyCounts { attackers = 0, defenders = 1 }
                   | a <= d = ArmyCounts { attackers = 1, defenders = 0 }
          roll _ _ = mempty
          sortedAttack = sortBy (flip compare) attack
          sortedDefend = sortBy (flip compare) defend

