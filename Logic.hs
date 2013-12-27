module Logic
(
 Task(..),
 Date(..),
 Time(..),
 Repeat(..),
 Year,
 Month,
 Day,
 Hour,
 Minute
)where

type Year = Int
type Month = Int
type Day = Int
type Hour = Int
type Minute = Int

data Date = NoDate | Date {year::Year, month::Month, day::Day} deriving (Show)
data Time = NoTime | Time {hour::Hour, minute::Minute} deriving (Show)
data Repeat = NoRepeat | EveryDay | EveryWeek | EveryMonth | EveryYear deriving (Show)
data Task = Task { name :: String
                     , date :: Date
                     , time :: Time
                     , repeatability :: Repeat
                     } deriving (Show)