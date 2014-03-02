module Types.SeasonInfo where
data SeasonInfo = SeasonInfo { season :: Integer
                             , episode :: Integer
                             } deriving (Show, Eq)