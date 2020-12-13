import Test.Hspec
  ( SpecWith,
    hspec,
  )
import qualified Test.Lib
import qualified Test.Parse

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  Test.Lib.tests
  Test.Parse.tests
