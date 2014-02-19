{-# LANGUAGE OverloadedStrings #-}

-- file Spec.hs
import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Parsers
import Types
import Data.Attoparsec.Text

main :: IO ()
main = hspec $ do
  describe "Parsers.season_info" $ do
    it "parses season and episode number" $
      (parseOnly seasonInfoParser "2.1") `shouldBe` Right (SeasonInfo 2 1)

  describe "Parsers.episode_info" $ do
    it "parses an episode info with title" $ do
      let episodeString = "{Michael & Sharon (Seattle, WA) (#1.9)}"
      let episodeInfo = Right (Episode "Michael & Sharon (Seattle, WA)" (SeasonInfo 1 9))
      (parseOnly episodeInfoParser episodeString) `shouldBe` episodeInfo

    it "parses an episode info without title" $ do
      let episodeString = "{(#4.9)}"
      let episodeInfo = Right (Episode "" (SeasonInfo 4 9))
      (parseOnly episodeInfoParser episodeString) `shouldBe` episodeInfo
-- "1 Girl 5 Gays" (2009) {(#4.9)}				2012