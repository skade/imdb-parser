module Types.VideoGame where
import Types.Simple

data VideoGame = VideoGame { title :: Title
                           , productionYear :: ProductionYear
                           , releaseYear :: Year
                           } deriving (Show, Eq)