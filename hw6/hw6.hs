{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-
Name: Steven Tomcavage
Collaborators: none 
Notes: 
-}

module HW06 where

import Data.Aeson
import Data.List
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T

-- Create a Market type that maps to the markets JSON file
data Market = Market {
                        marketname :: T.Text,
                        x          :: Float,
                        y          :: Float,
                        state      :: T.Text
                     }
    deriving (Show, Generic)
instance FromJSON Market

-- Here's some data for testing
testJSON :: B.ByteString
testJSON = "[ { \"fmid\":1005969, \"marketname\":\"Y Not Wednesday Farmers Market at Town Center\", \"website\":\"http://www.sandlercenter.org/index/ynotwednesdays\", \"street\":\"201 Market Street,\", \"city\":\"Virginia Beach\", \"county\":\"Virginia Beach\", \"state\":\"Virginia\", \"zip\":\"23462\", \"season1date\":\"June to August\", \"season1time\":\"Wed:5:00 PM - 8:00 PM;\", \"season2date\":\"\", \"season2time\":\"\", \"season3date\":\"\", \"season3time\":\"\", \"season4date\":\"\", \"season4time\":\"\", \"x\":-76.135361, \"y\":36.841885, \"location\":\"Other\", \"credit\":\"Y\", \"wic\":\"N\", \"wiccash\":\"N\", \"sfmnp\":\"N\", \"snap\":\"N\", \"bakedgoods\":\"Y\", \"cheese\":\"Y\", \"crafts\":\"N\", \"flowers\":\"Y\", \"eggs\":\"Y\", \"seafood\":\"Y\", \"herbs\":\"N\", \"vegetables\":\"Y\", \"honey\":\"Y\", \"jams\":\"Y\", \"maple\":\"N\", \"meat\":\"N\", \"nursery\":\"N\", \"nuts\":\"N\", \"plants\":\"N\", \"poultry\":\"N\", \"prepared\":\"Y\", \"soap\":\"Y\", \"trees\":\"N\", \"wine\":\"Y\", \"updatetime\":\"5/5/12 17:56\" }, { \"fmid\":1008044, \"marketname\":\"10:10 Farmers Market\", \"website\":\"http://www.1010farmersmarket.com\", \"street\":\"5960 Stewart Parkway\", \"city\":\"Douglasville\", \"county\":\"Douglas\", \"state\":\"Georgia\", \"zip\":\"30135\", \"season1date\":\"January to December\", \"season1time\":\"Thu:4:00 pm - 7:00 pm;\", \"season2date\":\"\", \"season2time\":\"\", \"season3date\":\"\", \"season3time\":\"\", \"season4date\":\"\", \"season4time\":\"\", \"x\":-84.7689, \"y\":33.7196, \"location\":\"Faith-based institution (e.g., church, mosque, synagogue, temple)\", \"credit\":\"Y\", \"wic\":\"N\", \"wiccash\":\"N\", \"sfmnp\":\"N\", \"snap\":\"N\", \"bakedgoods\":\"Y\", \"cheese\":\"Y\", \"crafts\":\"N\", \"flowers\":\"Y\", \"eggs\":\"N\", \"seafood\":\"N\", \"herbs\":\"Y\", \"vegetables\":\"Y\", \"honey\":\"Y\", \"jams\":\"Y\", \"maple\":\"N\", \"meat\":\"Y\", \"nursery\":\"N\", \"nuts\":\"N\", \"plants\":\"Y\", \"poultry\":\"N\", \"prepared\":\"Y\", \"soap\":\"Y\", \"trees\":\"N\", \"wine\":\"N\", \"updatetime\":\"7/18/12 13:51\" }]"

testValue :: Value
testValue = case decode testJSON :: Maybe Value of
                Nothing  -> error "Unable to decode testJSON"
                Just val -> val

-- Exercise 1: Function to convert Y/N strings to booleans
ynToBool :: Value -> Value
ynToBool val = case val of
    String "Y" -> Bool True
    String "N" -> Bool False
    Object o   -> Object $ fmap ynToBool o
    Array a    -> Array $ fmap ynToBool a
    _          -> val

-- Exercise 2: Convert ByteString containing JSON to Value (with error handling)
parseData :: B.ByteString -> Either String Value
parseData bstring = fmap ynToBool $ eitherDecode bstring

-- Exercise 3: Parse a JSON file using the Market datatype
parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets bstring = case parseData bstring of
                           Left err  -> Left err
                           Right val -> case fromJSON val of
                               Error err2  -> Left err2
                               Success suc -> Right suc

-- Exercise 4: Load the markets.json file
loadData :: IO [Market]
loadData = do bstring <- B.readFile "markets.json"
              case parseMarkets bstring of 
                  Left err      -> fail err
                  Right markets -> return markets

loadTestData :: [Market]
loadTestData = case parseMarkets testJSON of 
                  Left err      -> fail err
                  Right markets -> markets

-- Exercise 5: Create an ordered list datatype and monoid instance 
data OrdList a = OrdList { getOrdList :: [a] } 
    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty          = OrdList { getOrdList = [] }
    mappend ol1 ol2 = OrdList { getOrdList = sort $ getOrdList ol1 ++ getOrdList ol2 }

evens :: OrdList Integer
evens = OrdList [2,4,6]

odds :: OrdList Integer
odds = OrdList [1,3,5]

combined :: OrdList Integer
combined = evens <> odds

-- This function type is to simplify the code:
--    Given a string to search for and the JSON to search in, produce m
type Searcher m = T.Text -> [Market] -> m

-- Exercise 6: Create a way to search for a market by name
search :: Monoid m => (Market -> m) -> Searcher m
search mk_m s = go 
    where go [] = mempty
          go (mkt@(Market {marketname = name}) : mkts) 
            | s `T.isInfixOf` name = mk_m mkt <> go mkts
            | otherwise            = go mkts

-- Exercise 7: Function that returns the first search result
firstFound :: Searcher (Maybe Market)
firstFound = undefined

-- Exerise 8: Function that returns the last search result
lastFound :: Searcher (Maybe Market)
lastFound = undefined

-- Exercise 9: Function that retuns all search results
allFound :: Searcher [Market]
allFound = search (:[]) 

testAllFound :: Bool
testAllFound = length (allFound "Farmer" t)    == 2 &&
               length (allFound "Wednesday" t) == 1 &&
               null (allFound "Tuesday" t)
                    where t = loadTestData

-- Exercise 10: Function that returns number of search results
numberFound :: Searcher Int
numberFound = undefined

