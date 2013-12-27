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

data Date = NoDate | Date {year::Year, month::Month, day::Day}
data Time = NoTime | Time {hour::Hour, minute::Minute}
data Repeat = NoRepeat | EveryDay | EveryWeek | EveryMonth | EveryYear
data Task = Task { name :: String
                     , date :: Date
                     , time :: Time
                     , repeatability :: Repeat
                     , isCompleted::Bool
                     } deriving (Show)

-- overriden show funcions for our data types
instance Show Time where
    show NoTime = "Czas nieokreślony"
    show (Time hour minute) = (show hour) ++ ":" ++ (show minute)

instance Show Date where
    show NoDate = "Data nieokreślona"
    show (Date year month day) = (show  year) ++ "-" ++ (show month) ++ "-" ++ (show day)

instance Show Repeat where
    show NoRepeat = "Jednorazowe"
    show EveryDay = "Codzienne"
    show EveryWeek = "Cotygodniowe"
    show EveryMonth = "Comiesięczne"
    show EveryYear = "Coroczne"