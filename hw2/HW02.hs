{-
Name: Steven Tomcavage
Collaborators: none 
Notes: 
-}

module HW02 where

import Words
import Data.List
import Data.Function

-- Though a Scrabble hand is the same Haskell type as a Scrabble word, they
-- have different properties. Specifically, a hand is unordered whereas a word
-- is ordered. We denote this distinction by using a type synonym to talk
-- about hands, even though we could just say `String`.
type Hand = [Char]

-- A `Template` is like a word, but it has '?' characters in some places as
-- placeholders for letters from a player's hand. Because real words do not
-- have '?' characters, we use another type synonym to track this distinction.
type Template = String

-- A 'STemplate' is like a template, but it has markers to indicate four kinds
-- of special board locations: double-letter noted with 'D', triple-letter
-- noted with 'T', double-word noted with '2', and triple-word noted with '3'.
-- For matching, these behave just like '?' does -- they can be filled in with
-- any letter. But, when scoring, any letter played on a 'D' gets double its
-- value, and any letter played on a 'T' gets triple its value. If any square
-- in the template is a '2', the whole word's value is doubled; if any square
-- in the template is a '3', the whole word's score is tripled. If multiple of
-- these special squares are in the same word, the effects multiply.
type STemplate = Template

-- Can the letters in this hand be used to form the given word? 
formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (x:xs) hand 
    | x `elem` hand = formableBy xs (delete x hand)
    | otherwise     = False

-- Get a list of all the words formable by this hand 
wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

-- Can the letters in this hand form the given word to fit the given template? 
wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ []             = True
wordFitsTemplate (t:ts) hand (x:xs) 
    | length (t:ts) /= length (x:xs) = False
    | t == '?' && x `elem` hand      = wordFitsTemplate ts (delete x hand) xs
    | t /= '?' && x == t             = wordFitsTemplate ts hand xs
    | otherwise                      = False

-- Get a list of all the words formable by this hand for this template 
wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate template hand = filter (wordFitsTemplate template hand) allWords

-- Get the point value for a word 
scrabbleValueWord :: String -> Int
scrabbleValueWord string = sum $ map scrabbleValue string

-- Get list of highest scoring possible words from a given list of words 
bestWords :: [String] -> [String]
bestWords []    = []
bestWords words = [ word | (value, word) <- sorted, value == (fst $ head sorted) ] 
                    where sorted = reverse $ sortBy (compare `on` fst) [((scrabbleValueWord w), w) | w <- words] 

