module Types.Product where
import Types.Movie
import Types.VideoGame
import Types.Series

data Product = Cinema Movie
             | TV Movie
             | Video Movie
             | VideoGame VideoGame
             | Series Series
             deriving (Show, Eq)
