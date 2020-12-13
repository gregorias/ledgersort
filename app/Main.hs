import qualified Data.Text.IO as T
import Lib (sort)
import System.Exit (exitFailure)
import System.IO
  ( stderr,
    stdin,
    stdout,
  )

main :: IO ()
main = do
  journalText <- T.hGetContents stdin
  case sort journalText of
    Left err -> do
      T.hPutStrLn stderr err
      exitFailure
    Right sortedJournalText -> T.hPutStr stdout sortedJournalText
