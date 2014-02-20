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

data Expression = ConsExpr Expression Expression | TitleText Text | ProdYear ProductionYear deriving(Show,Eq)

movieHeadParser :: Parser Expression
movieHeadParser = do
    val <- pYear `eitherP` fragment
    either stop continue val
    where
      stop v = return $ ProdYear v
      continue v = do
        next <- movieHeadParser
        return $ ConsExpr (TitleText v) next
