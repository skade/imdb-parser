{-# LANGUAGE OverloadedStrings #-}

module Parsers (seasonInfoParser,
                episodeInfoParser,
                productionYearParser,
                broadcastInfoParser,
                seriesTitleParser,
                episodeParser,
                seriesParser,
                movieHeadParser,
                movieParser) where
import Types.SeasonInfo
import Types.Episode
import Types.Movie
import Types.Series
import Types.Simple
import Data.Attoparsec.Text.Lazy
import Data.Text
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Either
import Data.Char (isSpace)

dash = char '-'

seasonInfoParser :: Parser SeasonInfo
seasonInfoParser = SeasonInfo
                   <$> (decimal <* char '.')
                   <*> decimal

episodeInfoParser :: Parser (Title, SeasonInfo)
episodeInfoParser = createTuple
                    <$> (char '{' *> skipSpace *> title)
                    <*> (seasonInfoParser <* char ')' <* char '}')
    where
      createTuple title info = ((stripEnd (pack title)), info)
      title = manyTill anyChar (try (string "(#"))

productionYearParser :: Parser ProductionYear
productionYearParser = skipSpace *> char '(' *> decimal <* char ')'

broadcastYearParser :: Parser BroadcastInfo
broadcastYearParser = BroadcastYear <$> decimal

broadcastYearsParser :: Parser BroadcastInfo
broadcastYearsParser = BroadcastYears
                       <$> (decimal <* dash)
                       <*> (Just <$> decimal)

broadcastYearsParserOpenEnd :: Parser BroadcastInfo
broadcastYearsParserOpenEnd = createOpenEndYears <$> (decimal <* string "-????")
  where
    createOpenEndYears beg = BroadcastYears beg Nothing

broadcastInfoParser :: Parser BroadcastInfo
broadcastInfoParser = (broadcastYearsParser <|> broadcastYearsParserOpenEnd <|> broadcastYearParser)

seriesTitleParser :: Parser Title
seriesTitleParser = pack <$> (char '"' *> (manyTill anyChar (try (char '"'))))

seriesParser :: Parser Series
seriesParser = Series
               <$> (seriesTitleParser <* skipSpace)
               <*> (productionYearParser <* skipSpace)
               <*> (broadcastInfoParser <* endOfLine)
               <*> (many' episodeParser)

episodeParser :: Parser Episode
episodeParser = createEpisode
                <$> (seriesTitleParser *> skipSpace *> productionYearParser)
                <*> (skipSpace *> episodeInfoParser)
                <*> (skipSpace *> broadcastInfoParser <* endOfLine)
  where
    createEpisode prodYear (title,sinfo) byear = Episode title sinfo prodYear byear

fragment :: Parser Text
fragment = skipSpace *> takeTill isSpace

pYear :: Parser ProductionYear
pYear = skipSpace *> productionYearParser

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
movieHeadParser = construct <$> movieHeadParser'

releaseTypeParser :: Parser ReleaseType
releaseTypeParser = skipSpace *> (string "(TV)" >> return TV)
                              <|> (string "(VG)" >> return VideoGame)
                              <|> (string "(V)" >> return Video)
                              <|> (string "" >> return Cinema)

movieParser :: Parser Movie
movieParser = createMovie
              <$> movieHeadParser
              <*> (releaseTypeParser <* skipSpace)
              <*> (decimal <* endOfLine)
    where
      createMovie (t,p) r ry = Movie t p ry r
