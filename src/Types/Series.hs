module Types.Series where
import Types.Episode
import Types.Simple

data Series = Series { title :: Title
                     , productionYear :: ProductionYear
                     , broadcastYears :: BroadcastInfo
                     , episodes :: [Episode]
                     , isSuspended :: Bool
                     } deriving (Show, Eq)