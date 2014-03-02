module Types.Movie where
import Types.Simple

data Movie = Movie { title :: Title
                   , productionYear :: ProductionYear
                   , releaseYear :: Year
                   , releaseType :: ReleaseType
                   } deriving (Show, Eq)