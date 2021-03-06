module Types.Simple where
import Data.Text

type Title = Text
type Year = Integer
type ProductionYear = Maybe Year
type Index = Maybe Text

data BroadcastInfo = BroadcastYear (Maybe Year)
                   | BroadcastYears Year (Maybe Year)
                     deriving (Show, Eq)

data ReleaseType = Cinema | Video | TV | VideoGame deriving (Show, Eq)