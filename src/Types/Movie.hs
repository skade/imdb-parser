module Types.Movie where
import Types.Simple

data Movie = Movie { mtitle :: Title
                   , productionY :: ProductionYear
                   , releaseY :: Year
                   , releaseType :: ReleaseType
                   } deriving (Show, Eq)