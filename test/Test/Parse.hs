{-# LANGUAGE OverloadedStrings #-}

module Test.Parse (
  tests,
) where

import Data.Time (fromGregorian)
import Parse (parseJournal)
import Test.Hspec (
  Expectation,
  SpecWith,
  describe,
  it,
 )
import Test.Hspec.Expectations.Pretty (shouldBe)
import Types (DatedChunk (..), Journal (Journal), jDatedChunks, jPreamble, jSuffix)

tests :: SpecWith ()
tests = do
  describe "Parse" $ do
    describe "parse" $ do
      it "parses a journal" shouldParseJournal
      it "Keeps empty lines at the journal's end" shouldKeepSuffix

shouldParseJournal :: Expectation
shouldParseJournal =
  parseJournal journal
    `shouldBe` Right
      ( Journal
          { jPreamble = preamble
          , jDatedChunks =
              [ DatedChunk (fromGregorian 2019 4 22) "P 2019/04/22 CHF HRK 6.52\n"
              , DatedChunk
                  (fromGregorian 2020 1 1)
                  "2020/01/01 * Patreon\n\
                  \  Assets:Liquid:Revolut:EUR  EUR -8.05\n\
                  \  Expenses:Leisure\n"
              ]
          , jSuffix = ""
          }
      )
 where
  journal =
    "; -*- ledger -*-\n\
    \; This line instruct vim-ledger to enable the filetype\n\
    \; vim:filetype=ledger\n\
    \;\n\
    \; ledger -X CHF -f wallet.dat b\n\
    \\n\
    \account Assets:Investments\n\
    \note Liquid financial assets that appreciate in value.\n\
    \\n\
    \P 2019/04/22 CHF HRK 6.52\n\
    \\n\
    \2020/01/01 * Patreon\n\
    \  Assets:Liquid:Revolut:EUR  EUR -8.05\n\
    \  Expenses:Leisure"
  preamble =
    "; -*- ledger -*-\n\
    \; This line instruct vim-ledger to enable the filetype\n\
    \; vim:filetype=ledger\n\
    \;\n\
    \; ledger -X CHF -f wallet.dat b\n\
    \\n\
    \account Assets:Investments\n\
    \note Liquid financial assets that appreciate in value.\n\
    \\n"

shouldKeepSuffix :: Expectation
shouldKeepSuffix =
  parseJournal journal
    `shouldBe` Right
      ( Journal
          { jPreamble = ""
          , jDatedChunks = [DatedChunk (fromGregorian 2019 4 22) "P 2019/04/22 CHF HRK 6.52\n"]
          , jSuffix = "\n"
          }
      )
 where
  journal = "P 2019/04/22 CHF HRK 6.52\n\n"
