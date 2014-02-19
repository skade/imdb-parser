{-# LANGUAGE OverloadedStrings #-}

module Types where
import Data.Text

data SeasonInfo = SeasonInfo { season :: Integer
                             , episode :: Integer
                             } deriving (Show, Eq)

data Episode = Episode { title :: Text
                       , season_info :: SeasonInfo
                       } deriving (Show, Eq)

data Series = Series  {  name :: Text
                       , episodes :: [Episode]
                       } deriving (Show)
