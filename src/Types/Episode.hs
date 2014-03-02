module Types.Episode where
import Types.Simple
import Types.SeasonInfo

data Episode = Episode { title :: Title
                       , seasonInfo :: SeasonInfo
                       , productionYear :: ProductionYear
                       , broadcastYear :: BroadcastInfo
                       , isSuspended :: Bool
                       } deriving (Show, Eq)