{-# LANGUAGE OverloadedStrings #-}

module Parsers where
import Types
import Data.Attoparsec.Text
import Data.Text

seasonInfoParser :: Parser SeasonInfo
seasonInfoParser = do
    sea <- decimal
    char '.'
    ep <- decimal
    return $ SeasonInfo sea ep

episodeInfoParser :: Parser Episode
episodeInfoParser = do
    char '{'
    skipSpace
    title <- (manyTill anyChar (try (string "(#")))
    seasonInfo <- seasonInfoParser
    char ')'
    char '}'
    return $ Episode (stripEnd (pack title)) seasonInfo