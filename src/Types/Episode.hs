module Types.Episode where
import Types.Simple
import Types.SeasonInfo

data Episode = Episode { title :: Title
                       , season_info :: SeasonInfo
                       , prodYear :: ProductionYear
                       , broadcastYear :: BroadcastInfo
                       } deriving (Show, Eq)