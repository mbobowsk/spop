module Communication
( menu
) where

import Logic
import qualified Data.Map as Map

import System.Locale
import Data.Time
import Data.Time.Format
import Data.Maybe

menu tasks = do
    putStrLn "\nMENU GŁÓWNE"
    putStrLn "1 - dodaj zadanie"
    putStrLn "2 - usuń zadanie"
    putStrLn "3 - wyswietl zadania"
    putStrLn "4 - podaj czas systemowy"
    putStrLn "0 - wyjdź"
    c <- getLine
    case (read c::Int) of
        1 ->do
            putStrLn "choice is 1"
            newTask <- makeTask
            menu (newTask:tasks)
        2 -> do
            putStrLn "choice is 2"
            menu tasks
        3 -> do
            putStrLn "Wyświetlanie zadań"
            print tasks
            menu tasks
        4 -> do
            time <- getTimeNow
            putStrLn ("Czas systemowy: " ++ time)
            menu tasks
        0 -> putStrLn "choice is 3"
        _ ->do
            putStrLn "Nieprawidłowy wybór"
            menu tasks

makeTask = do
    putStrLn "Podaj nazwe zadania:"
    taskName <- getLine
    rep <- getRepeatability
    date <- getSafeDate
    return $ Task {name = taskName, time = date, repeatability = rep, isCompleted = False}

-- gets from user repeatability of task
getRepeatability = do
    putStrLn "Powtarzalność zadania:"
    putStrLn "1 - Jednorazowe"
    putStrLn "2 - Codziennie"
    putStrLn "3 - Co tydzień"
    putStrLn "4 - Co miesiąc"
    putStrLn "5 - Co rok"
    choice <- getLine
    case read (choice)::Int of
        1 -> return NoRepeat
        2 -> return EveryDay
        3 -> return EveryWeek
        4 -> return EveryMonth
        5 -> return EveryYear
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            getRepeatability

getTimeNow = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%M") getCurrentTime

getSafeDate = do
	date <- getDate
	if (isNothing date)
		then	getSafeDate
		else	return (fromJust date)

getDate = do
	putStrLn "Podaj datę w formacie yyyy-mm-dd HH:MM:"
	dateString <- getLine
	let	parse = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" dateString :: Maybe UTCTime
	return parse

addDay (UTCTime day time) = UTCTime (addDays 1 day) time
addWeek (UTCTime day time) = UTCTime (addDays 7 day) time
addMonth (UTCTime day time) = UTCTime (addDays 30 day) time
addYear (UTCTime day time) = UTCTime (addDays 365 day) time


