module Logic
(
 Task(..),
 TaskBook(..),
 Repeat(..),
)where

import Data.Time

data Repeat = NoRepeat | EveryDay | EveryWeek | EveryMonth | EveryYear deriving (Eq)
data Task = Task { name :: String
                     , time :: UTCTime
                     , repeatability :: Repeat
                     , isCompleted::Bool
                     } deriving (Show)

data TaskBook = TaskBook {date::UTCTime,
                            tasks::[Task]}

-- overriden show funcions for our data types
instance Show Repeat where
    show NoRepeat = "Jednorazowe"
    show EveryDay = "Codzienne"
    show EveryWeek = "Cotygodniowe"
    show EveryMonth = "Comiesięczne"
    show EveryYear = "Coroczne"

instance Show TaskBook where
    show (TaskBook date tasks) = (show date) ++ (showTasks tasks)

instance Eq Task where
    (Task name time repeatability isCompleted) == (Task name2 time2 repeatability2 isCompleted2) =
        if( name == name2 && time == time2 && repeatability  == repeatability2 && isCompleted == isCompleted2) then True
        else False
    x /= y = not (x == y)

showTasks [] = "\n"
showTasks (first:rest) = (showTasks rest) ++ (show $ 1 + length rest) ++ " - " ++ (show first) ++ "\n"
