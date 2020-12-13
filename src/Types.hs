module Types
  ( DatedChunk (..),
    Journal (..),
  )
where

import Data.Text (Text)
import Data.Time (Day)

data DatedChunk = DatedChunk
  { dcDate :: Day,
    dcContent :: Text
  }
  deriving (Eq, Show)

data Journal = Journal
  { jPreamble :: Text,
    jDatedChunks :: [DatedChunk]
  }
  deriving (Eq, Show)
