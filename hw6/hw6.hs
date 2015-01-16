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

testJSON :: B.ByteString
testJSON = "[{ \"fmid\":1005969, \"marketname\":\"Y Not Wednesday Farmers Market at Town Center\", \"website\":\"http://www.sandlercenter.org/index/ynotwednesdays\", \"street\":\"201 Market Street,\", \"city\":\"Virginia Beach\", \"county\":\"Virginia Beach\", \"state\":\"Virginia\", \"zip\":\"23462\", \"season1date\":\"June to August\", \"season1time\":\"Wed:5:00 PM - 8:00 PM;\", \"season2date\":\"\", \"season2time\":\"\", \"season3date\":\"\", \"season3time\":\"\", \"season4date\":\"\", \"season4time\":\"\", \"x\":-76.135361, \"y\":36.841885, \"location\":\"Other\", \"credit\":\"Y\", \"wic\":\"N\", \"wiccash\":\"N\", \"sfmnp\":\"N\", \"snap\":\"N\", \"bakedgoods\":\"Y\", \"cheese\":\"Y\", \"crafts\":\"N\", \"flowers\":\"Y\", \"eggs\":\"Y\", \"seafood\":\"Y\", \"herbs\":\"N\", \"vegetables\":\"Y\", \"honey\":\"Y\", \"jams\":\"Y\", \"maple\":\"N\", \"meat\":\"N\", \"nursery\":\"N\", \"nuts\":\"N\", \"plants\":\"N\", \"poultry\":\"N\", \"prepared\":\"Y\", \"soap\":\"Y\", \"trees\":\"N\", \"wine\":\"Y\", \"updatetime\":\"5/5/12 17:56\"}]"

