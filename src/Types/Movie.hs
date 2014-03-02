module Types.Movie where
import Types.Simple

data Movie = Movie { title :: Title
                   , productionYear :: ProductionYear
                   , releaseYear :: Maybe Year
                   , isSuspended :: Bool
                   } deriving (Show, Eq)