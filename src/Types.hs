{-# LANGUAGE OverloadedStrings #-}

module Types where
import Data.Text

data SeasonInfo = SeasonInfo { season :: Integer
                             , episode :: Integer
                             } deriving (Show, Eq)

data Episode = Episode { title :: Title
                       , season_info :: SeasonInfo
                       , prodYear :: ProductionYear
                       , broadcastYear :: BroadcastInfo
                       } deriving (Show, Eq)

data Series = Series  {  name :: Title
                       , episodes :: [Episode]
                       , productionYear :: ProductionYear
                       , broadcastYears :: BroadcastInfo
                       } deriving (Show, Eq)

type Title = Text
type Year = Integer
type ProductionYear = Year

data BroadcastInfo = BroadcastYear Year
                   | BroadcastYears Year (Maybe Year)
                     deriving (Show, Eq)
