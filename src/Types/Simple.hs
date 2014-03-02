module Types.Simple where
import Data.Text

type Title = Text
type Year = Integer
type ProductionYear = Year

data BroadcastInfo = BroadcastYear Year
                   | BroadcastYears Year (Maybe Year)
                     deriving (Show, Eq)

data ReleaseType = Cinema | Video | TV | VideoGame deriving (Show, Eq)