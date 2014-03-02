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


type Title = Text
type Year = Integer
type ProductionYear = Year

data BroadcastInfo = BroadcastYear Year
                   | BroadcastYears Year (Maybe Year)
                     deriving (Show, Eq)

data Series = Series { name :: Title
                     , productionYear :: ProductionYear
                     , broadcastYears :: BroadcastInfo
                     , episodes :: [Episode]
                     } deriving (Show, Eq)

data Movie = Movie { mtitle :: Title
                   , productionY :: ProductionYear
                   , releaseY :: Year
                   , releaseType :: ReleaseType
                   } deriving (Show, Eq)

data ReleaseType = Cinema | Video | TV | VideoGame deriving (Show, Eq)
