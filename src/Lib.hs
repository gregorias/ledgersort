{-# LANGUAGE OverloadedStrings #-}

module Lib (
  sort,
) where

import Data.Bifunctor (Bifunctor (first))
import Data.Function (on)
import Data.List (sortBy)
import Data.Text (Text)
import qualified Data.Text as T
import Parse (parseJournal)
import Types (Journal (..), dcContent, dcDate)

showJournal :: Journal -> Text
showJournal (Journal preamble datedChunks suffix) =
  T.concat
    [ preamble
    , T.intercalate
        "\n"
        (map dcContent datedChunks)
    , suffix
    ]

sortJournal :: Journal -> Journal
sortJournal j = j{jDatedChunks = sortedChunks}
 where
  unsortedChunks = jDatedChunks j
  sortedChunks = sortBy (compare `on` dcDate) unsortedChunks

-- | Parses and sorts a journal
sort :: Text -> Either Text Text
sort journalText = do
  journal <- first T.pack $ parseJournal journalText
  return . showJournal . sortJournal $ journal
