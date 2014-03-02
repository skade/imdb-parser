{-# LANGUAGE OverloadedStrings #-}

module Parsers (seasonInfoParser,
                episodeInfoParser,
                productionYearParser,
                broadcastInfoParser,
                seriesTitleParser,
                episodeParser,
                seriesParser,
                movieHeadParser,
                movieishParser) where
import Types.SeasonInfo
import Types.Episode
import Types.Movie
import Types.Series
import qualified Types.VideoGame as V
import qualified Types.Product as P
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
productionYearParser =   (Just <$> (skipSpace *> char '(' *> decimal <* (char ')' <|> char '/')))
                      <|> ((skipSpace *> string "(????)") >> return Nothing)

broadcastYearParser :: Parser BroadcastInfo
broadcastYearParser = BroadcastYear <$> ((Just <$> decimal) <|> (string "????" >> return Nothing))

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

seriesParser :: Parser P.Product
seriesParser = P.Series <$> (createSeries
               <$> (seriesTitleParser <* skipSpace)
               <*> (productionYearParser <* skipSpace)
               <*> (suspensionParser <* skipSpace)
               <*> (broadcastInfoParser <* endOfLine)
               <*> (many' episodeParser))
  where
    createSeries t p s b e = Series t p b e s

episodeParser :: Parser Episode
episodeParser = createEpisode
                <$> (seriesTitleParser *> skipSpace *> productionYearParser)
                <*> (skipSpace *> episodeInfoParser)
                <*> (skipSpace *> suspensionParser)
                <*> (skipSpace *> broadcastInfoParser <* endOfLine)
  where
    createEpisode prodYear (title,sinfo) s byear = Episode title sinfo prodYear byear s

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
releaseTypeParser = skipSpace *> ((string "(TV)" >> return TV)
                              <|> (string "(VG)" >> return VideoGame)
                              <|> (string "(V)" >> return Video)
                              <|> (string "" >> return Cinema))

movieishParser :: Parser P.Product
movieishParser = createProduct
              <$> movieHeadParser
              <*> (releaseTypeParser <* skipSpace)
              <*> (suspensionParser <* skipSpace)
              <*> (((Just <$> decimal) <|> (string "????" >> return Nothing)) <* endOfLine)
    where
      createProduct (t,p) Cinema s ry = P.Cinema $ Movie t p ry s
      createProduct (t,p) TV s ry = P.TV $ Movie t p ry s
      createProduct (t,p) Video s ry = P.Video $ Movie t p ry s
      createProduct (t,p) VideoGame s ry = P.VideoGame $ V.VideoGame t p ry s

suspensionParser :: Parser Bool
suspensionParser = skipSpace *> ((string "{{SUSPENDED}}" >> return True)
                                 <|> (string "" >> return False))