{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module parses input text into a journal
module Parse
  ( parseJournal,
  )
where

import Control.Monad (guard)
import Data.Bifunctor (Bifunctor (first))
import Data.Functor (($>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Data.Void (Void)
import Text.Megaparsec
  ( Parsec,
    chunk,
    eof,
    errorBundlePretty,
    label,
    lookAhead,
    match,
    noneOf,
    optional,
    parse,
    sepBy,
    single,
    some,
    someTill,
    someTill_,
    try,
    (<|>),
  )
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char (newline, printChar, spaceChar)
import Types
  ( DatedChunk (..),
    Journal (..),
  )

type Parser = Parsec Void Text

emptyLine :: Parser Text
emptyLine = T.singleton <$> newline

nonEmptyLine :: Parser Text
nonEmptyLine = do
  (symbols, end) <-
    someTill_
      (noneOf "\n")
      (newline <|> (eof $> '\n'))
  return $ T.append (T.pack symbols) (T.singleton end)

commentLine :: Parser Text
commentLine = label "a comment" $ do
  start <- T.singleton <$> single ';'
  comment <- MP.takeWhileP Nothing (/= '\n')
  nl <- (T.singleton <$> try newline) <|> pure ""
  return $ T.concat [start, comment, nl]

date :: Parser Day
date = label "date" $ do
  dateString <- someTill printChar spaceChar
  label "date string" $
    parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString
      <|> parseTimeM True defaultTimeLocale "%Y/%m/%d" dateString

datedLine :: Parser Day
datedLine = label "a line with a date" $ do
  let pricePrefix = do
        p <- chunk "P"
        spaces <- T.pack <$> MP.some spaceChar
        return $ T.append p spaces
  optional pricePrefix
  day <- date
  MP.manyTill
    printChar
    ((T.singleton <$> newline) <|> (eof $> ""))
  return day

datedChunk :: Parser DatedChunk
datedChunk = label "dated chunk (transaction or price statement)" $ do
  comments <- T.concat <$> MP.many commentLine
  (datedLineString, day) <- match datedLine
  nels <- T.concat <$> MP.many nonEmptyLine
  return $ DatedChunk day (T.concat [comments, datedLineString, nels])

undatedChunk :: Parser Text
undatedChunk = do
  isDated <- (lookAhead (try datedChunk) $> True) <|> pure False
  guard (not isDated)
  (text, _) <- match $ some nonEmptyLine
  return text

undatedChunksOrEmptyLines :: Parser Text
undatedChunksOrEmptyLines =
  MP.choice [try emptyLine, try undatedChunk]

preamble :: Parser Text
preamble = label "preamble (comments and configuration)" $ do
  chunks <- MP.many undatedChunksOrEmptyLines
  return $ T.concat chunks

journalParser :: Parser Journal
journalParser = do
  preamble <- preamble
  datedChunks <- sepBy datedChunk (try $ some emptyLine)
  eof
  return $ Journal preamble datedChunks

parseJournal :: Text -> Either String Journal
parseJournal input = first errorBundlePretty $ parse journalParser "" input
