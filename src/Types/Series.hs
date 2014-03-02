module Types.Series where
import Types.Episode
import Types.Simple

data Series = Series { title :: Title
                     , productionYear :: ProductionYear
                     , broadcastYears :: BroadcastInfo
                     , episodes :: [Episode]
                     } deriving (Show, Eq)