module Types.VideoGame where
import Types.Simple

data VideoGame = VideoGame { title :: Title
                           , productionYear :: ProductionYear
                           , releaseYear :: Maybe Year
                           , isSuspended :: Bool
                           } deriving (Show, Eq)