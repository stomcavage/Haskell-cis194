{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-
Name: Steven Tomcavage
Collaborators: none 
Notes: 
-}

module HW06 where

import Data.Aeson
import Data.Monoid
import GHC.Generics

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

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
testJSON = "[{ \"fmid\":1005969, \"marketname\":\"Y Not Wednesday Farmers Market at Town Center\", \"website\":\"http://www.sandlercenter.org/index/ynotwednesdays\", \"street\":\"201 Market Street,\", \"city\":\"Virginia Beach\", \"county\":\"Virginia Beach\", \"state\":\"Virginia\", \"zip\":\"23462\", \"season1date\":\"June to August\", \"season1time\":\"Wed:5:00 PM - 8:00 PM;\", \"season2date\":\"\", \"season2time\":\"\", \"season3date\":\"\", \"season3time\":\"\", \"season4date\":\"\", \"season4time\":\"\", \"x\":-76.135361, \"y\":36.841885, \"location\":\"Other\", \"credit\":\"Y\", \"wic\":\"N\", \"wiccash\":\"N\", \"sfmnp\":\"N\", \"snap\":\"N\", \"bakedgoods\":\"Y\", \"cheese\":\"Y\", \"crafts\":\"N\", \"flowers\":\"Y\", \"eggs\":\"Y\", \"seafood\":\"Y\", \"herbs\":\"N\", \"vegetables\":\"Y\", \"honey\":\"Y\", \"jams\":\"Y\", \"maple\":\"N\", \"meat\":\"N\", \"nursery\":\"N\", \"nuts\":\"N\", \"plants\":\"N\", \"poultry\":\"N\", \"prepared\":\"Y\", \"soap\":\"Y\", \"trees\":\"N\", \"wine\":\"Y\", \"updatetime\":\"5/5/12 17:56\"}]"

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

