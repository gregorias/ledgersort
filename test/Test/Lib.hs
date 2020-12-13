{-# LANGUAGE OverloadedStrings #-}

module Test.Lib
  ( tests,
  )
where

import qualified Data.Text as T
import Lib (sort)
import Test.Hspec (Expectation, SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Lib" $ do
    describe "sort" $ do
      it "parses and sorts a journal" $ do
        shouldSortJournal

shouldSortJournal :: Expectation
shouldSortJournal = do
  sort journal `shouldBe` Right expectedJournal
  where
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
    journal =
      T.append
        preamble
        "P 2020/04/22 CHF HRK 6.52\n\
        \P 2019/01/22 CHF HRK 6.52\n\
        \\n\
        \; comment\n\
        \2020/01/01 * Patreon\n\
        \  Assets:Liquid:Revolut:EUR  EUR -8.05\n\
        \  Expenses:Leisure"
    expectedJournal =
      T.append
        preamble
        "; comment\n\
        \2020/01/01 * Patreon\n\
        \  Assets:Liquid:Revolut:EUR  EUR -8.05\n\
        \  Expenses:Leisure\n\
        \\n\
        \P 2020/04/22 CHF HRK 6.52\n\
        \P 2019/01/22 CHF HRK 6.52\n"
