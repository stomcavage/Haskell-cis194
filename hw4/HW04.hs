{-# OPTIONS_GHC -Wall #-}
{-
Name: Steven Tomcavage
Collaborators: none 
Notes: 
-}

module HW04 where

import BST
import Data.Char
import Data.List
import Data.Maybe
import Safe

-- Exercise 13: Wring an insert method for a binary search tree
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ item Leaf = Node Leaf item Leaf
insertBST cmp item (Node left x right) 
    = case item `cmp` x of
        LT -> Node (insertBST cmp item left) x right
        EQ -> Node (insertBST cmp item left) x right
        GT -> Node left x (insertBST cmp item right)

-- Exercise 14: Check that all strings contain only capitalized words
allCaps :: [String] -> Bool
allCaps = all (\s -> maybe False isUpper $ headMay s) 

-- Exercise 15: Drop the trailing whitespace from a string
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace 

-- Exercise 16: Get the first letters from a list of strings
firstLetters :: [String] -> [Char]
firstLetters = mapMaybe listToMaybe 

-- Exercise 17: Render a bracketed string from a list of strings
asList :: [String] -> String
asList strings = "[" ++ (concat $ intersperse ", " strings) ++ "]"
