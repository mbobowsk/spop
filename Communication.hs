module Communication
( menu
) where

import Logic
import qualified Data.Map as Map

menu tasks = do
    putStrLn "\nMENU GŁÓWNE"
    putStrLn "1 - dodaj zadanie"
    putStrLn "2 - usuń zadanie"
    putStrLn "3 - wyswietl zadania"
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
        0 -> putStrLn "choice is 3"
        _ ->do
            putStrLn "Nieprawidłowy wybór"
            menu tasks

makeTask = do
    putStrLn "Podaj nazwe zadania:"
    taskName <- getLine
--    putStrLn "Podaj date (yyyy/mm/dd np 2013/01/13):"
--    dateString <- getLine
--    putStrLn "Podaj termin zadania (hh:mm np. 15:30):"
--    timeString <- getLine
    rep <- getRepeatability
    return $ Task {name = taskName, date = Date 2013 01 12, time = Time 12 20, repeatability = rep, isCompleted = False}

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