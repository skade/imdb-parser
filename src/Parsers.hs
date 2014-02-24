{-# LANGUAGE OverloadedStrings #-}

module Parsers where
import Types
import Data.Attoparsec.Text
import Data.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Either
import Data.Char (isSpace)

seasonInfoParser :: Parser SeasonInfo
seasonInfoParser = do
    sea <- decimal
    char '.'
    ep <- decimal
    return $ SeasonInfo sea ep

episodeInfoParser :: Parser (Title, SeasonInfo)
episodeInfoParser = do
    char '{'
    skipSpace
    title <- (manyTill anyChar (try (string "(#")))
    seasonInfo <- seasonInfoParser
    char ')'
    char '}'
    return $ ((stripEnd (pack title)), seasonInfo)

productionYearParser :: Parser ProductionYear
productionYearParser = do
    skipSpace
    char '('
    year <- decimal
    char ')'
    return year

broadcastYearParser :: Parser BroadcastInfo
broadcastYearParser = do
    y <- decimal
    return $ BroadcastYear y

broadcastYearsParser :: Parser BroadcastInfo
broadcastYearsParser = do
    beg <- decimal
    char '-'
    end <- decimal
    return $ BroadcastYears beg (Just end)

broadcastYearsParserOpenEnd :: Parser BroadcastInfo
broadcastYearsParserOpenEnd = do
    beg <- decimal
    string "-????"
    return $ BroadcastYears beg Nothing

broadcastInfoParser :: Parser BroadcastInfo
broadcastInfoParser = (broadcastYearsParser <|> broadcastYearsParserOpenEnd <|> broadcastYearParser)

seriesTitleParser :: Parser Title
seriesTitleParser = do
    char '"'
    title <- (manyTill anyChar (try (char '"')))
    return $ pack title

seriesParser :: Parser Series
seriesParser = do
    t <- seriesTitleParser
    skipSpace
    prod <- productionYearParser
    skipSpace
    byears <- broadcastInfoParser
    endOfLine
    episodes <- many' episodeParser
    return $ Series t episodes prod byears

episodeParser :: Parser Episode
episodeParser = do
    seriesTitleParser
    skipSpace
    prod <- productionYearParser
    skipSpace
    (t,sinfo) <- episodeInfoParser
    skipSpace
    byear <- broadcastInfoParser
    endOfLine
    return $ Episode t sinfo prod byear

fragment :: Parser Text
fragment = do
    skipSpace
    t <- takeTill isSpace
    return t

pYear :: Parser ProductionYear
pYear = do
    skipSpace
    year <- productionYearParser
    return year

data MovieHead = Cons Text MovieHead | Final ProductionYear deriving(Show,Eq)

movieHeadParser' :: Parser MovieHead
movieHeadParser' = (Final <$> pYear) <|>
                   (Cons <$> (fragment) <*> movieHeadParser')

construct :: MovieHead -> (Title, ProductionYear)
construct e = (strip(title e), year e)
    where
      title (Cons t e) = (t `snoc` ' ') `append` (title e)
      title (Final y) = ""
      year (Cons _ e) = year e
      year (Final y) = y

movieHeadParser :: Parser (Title, ProductionYear)
movieHeadParser = do
    expr <- movieHeadParser'
    return $ construct expr

releaseTypeParser :: Parser ReleaseType
releaseTypeParser = do
      skipSpace
      (string "(TV)" >> return TV)
      <|> (string "(VG)" >> return VideoGame)
      <|> (string "(V)" >> return Video)
      <|> (string "" >> return Cinema)

movieParser :: Parser Movie
movieParser = do
    (t,p) <- movieHeadParser
    r <- releaseTypeParser
    skipSpace
    ry <- decimal
    endOfLine
    return $ Movie t p ry r
