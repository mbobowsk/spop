module Logic
(
 Task(..),
 Repeat(..)
)where

import Data.Time

data Repeat = NoRepeat | EveryDay | EveryWeek | EveryMonth | EveryYear
data Task = Task { name :: String
                     , time :: UTCTime
                     , repeatability :: Repeat
                     , isCompleted::Bool
                     } deriving (Show)

-- overriden show funcions for our data types
instance Show Repeat where
    show NoRepeat = "Jednorazowe"
    show EveryDay = "Codzienne"
    show EveryWeek = "Cotygodniowe"
    show EveryMonth = "Comiesięczne"
    show EveryYear = "Coroczne"
