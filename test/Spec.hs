{-# LANGUAGE OverloadedStrings #-}

-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Parsers
import Types.SeasonInfo
import Types.Episode
import Types.Simple
import Types.Movie
import Types.Series
import qualified Types.Product as P
import qualified Types.VideoGame as V

import Data.Attoparsec.Text

main :: IO ()
main = hspec $ do
  describe "Parsers.season_info" $ do
    it "parses season and episode number" $
      (parseOnly seasonInfoParser "2.1") `shouldBe` Right (SeasonInfo 2 1)

  describe "Parsers.episode_info" $ do
    it "parses an episode info with title" $ do
      let episodeString = "{Michael & Sharon (Seattle, WA) (#1.9)}"
      let episodeInfo = Right ("Michael & Sharon (Seattle, WA)", (SeasonInfo 1 9))
      (parseOnly episodeInfoParser episodeString) `shouldBe` episodeInfo

    it "parses an episode info without title" $ do
      let episodeString = "{(#4.9)}"
      let episodeInfo = Right ("", (SeasonInfo 4 9))
      (parseOnly episodeInfoParser episodeString) `shouldBe` episodeInfo

  describe "Parsers.productionYearParser" $ do
    it "parses a year with brackets" $ do
      (parseOnly productionYearParser "(1983)") `shouldBe` Right (Just 1983)

  describe "Parsers.productionYearParser" $ do
    it "parses an unknown production year" $ do
      (parseOnly productionYearParser "(????)") `shouldBe` Right Nothing

  describe "Parsers.broadcastInfoParser" $ do
    it "parses a begin and end, seperated by a dash" $ do
      let broadCastYears = Right (BroadcastYears 1994 Nothing)
      (parseOnly broadcastInfoParser "1994-????") `shouldBe` broadCastYears

    it "parses a begin and end, seperated by a dash" $ do
      let broadCastYears = Right (BroadcastYears 1994 (Just 1995))
      (parseOnly broadcastInfoParser "1994-1995") `shouldBe` broadCastYears

    it "parses a year without brackets" $ do
      (parseOnly broadcastInfoParser "1983") `shouldBe` Right (BroadcastYear 1983)

  describe "Parsers.seriesTitleParser" $ do
    it "parses a title within '\"'" $ do
      (parseOnly seriesTitleParser "\"!Next?\"") `shouldBe` Right "!Next?"

  describe "Parser.episodeParser" $ do
    it "parses an episode line" $ do
      let input = "\"#1 Single\" (2006) {Cats and Dogs (#1.4)} \t\t\t 2006\n"
      let result = Right (Episode "Cats and Dogs" (SeasonInfo 1 4) (Just 2006) (BroadcastYear 2006))
      (parseOnly episodeParser input) `shouldBe` result

  describe "Parser.seriesTitleParser" $ do
    it "parses a series line" $ do
      let input = "\"!Next?\" (1994) \t\t\t\t 1994-1995\n"
      let result = Right (P.Series (Series "!Next?" (Just 1994) (BroadcastYears 1994 (Just 1995)) []))
      (parseOnly seriesParser input) `shouldBe` result

    it "parses a series line with a single year" $ do
      let input = "\"!Next?\" (1994) \t\t\t\t 1994\n"
      let result = Right (P.Series (Series "!Next?" (Just 1994) (BroadcastYear 1994) []))
      (parseOnly seriesParser input) `shouldBe` result

    it "parses a series line and an episodes line" $ do
      let input = "\"#1 Single\" (2006) \t\t\t\t 2006-????\n\"#1 Single\" (2006) {Cats and Dogs (#1.4)} \t\t\t 2006\n"
      let result = Right (P.Series (Series "#1 Single" (Just 2006) (BroadcastYears 2006 Nothing) [Episode "Cats and Dogs" (SeasonInfo 1 4) (Just 2006) (BroadcastYear 2006)]))
      (parseOnly seriesParser input) `shouldBe` result

    it "parses a series line and an episodes line and doesn't continue to the next line" $ do
      let input = "\"#1 Single\" (2006) \t\t\t\t 2006-????\n\"#1 Single\" (2006) {Cats and Dogs (#1.4)} \t\t\t 2006\n\"#7DaysLater\" (2013) \t\t\t\t 2013-????"
      let result = Right (P.Series (Series "#1 Single" (Just 2006) (BroadcastYears 2006 Nothing) [Episode "Cats and Dogs" (SeasonInfo 1 4) (Just 2006) (BroadcastYear 2006)]))
      (parseOnly seriesParser input) `shouldBe` result

  describe "many' Parsers.seriesParser" $ do
    it "parses multiple series" $ do
      let input = "\"#1 Single\" (2006) \t\t\t\t 2006-????\n\"#1 Single\" (2006) {Cats and Dogs (#1.4)} \t\t\t 2006\n\"#7DaysLater\" (2013) \t\t\t\t 2013-????\n"
      let result = Right [P.Series (Series "#1 Single" (Just 2006) (BroadcastYears 2006 Nothing) [Episode "Cats and Dogs" (SeasonInfo 1 4) (Just 2006) (BroadcastYear 2006)]), (P.Series (Series "#7DaysLater" (Just 2013) (BroadcastYears 2013 Nothing) []))]
      (parseOnly (many' seriesParser) input) `shouldBe` result

  describe "Parser.movieHeadParser" $ do
    it "parses this pesky head of a movie line correctly" $ do
      let input = "Libertad (Mar del Plata) (1999)"
      let result = Right ("Libertad (Mar del Plata)", Just 1999)
      (parseOnly movieHeadParser input) `shouldBe` result

  describe "Parsers.movieishParser" $ do
    it "parses a TV movie" $ do
      let input = "Libertad (Mar del Plata) (1999) (TV) \t\t\t\t 1999\n"
      let result = Right (P.TV (Movie "Libertad (Mar del Plata)" (Just 1999) 1999))
      (parseOnly movieishParser input) `shouldBe` result

    it "parses a cinema movie" $ do
      let input = "Fight Club (1999) \t\t\t\t 1999\n"
      let result = Right (P.Cinema (Movie "Fight Club" (Just 1999) 1999))
      (parseOnly movieishParser input) `shouldBe` result

    it "parses a videogame" $ do
      let input = "Zool 2 (1993) (VG) \t\t\t\t\t 1993\n"
      let result = Right (P.VideoGame (V.VideoGame "Zool 2" (Just 1993) 1993))
      (parseOnly movieishParser input) `shouldBe` result
