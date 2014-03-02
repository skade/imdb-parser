module Types.Series where
import Types.Episode
import Types.Simple

data Series = Series { name :: Title
                     , productionYear :: ProductionYear
                     , broadcastYears :: BroadcastInfo
                     , episodes :: [Episode]
                     } deriving (Show, Eq)