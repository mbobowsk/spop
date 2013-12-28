module Communication
( menu,
  makeTask
) where

import Logic

import System.Locale
import Data.Time
import Data.Time.Format
import Data.Maybe
import System.IO
import System.Directory

menu (TaskBook date tasks) = do
    putStrLn "\nMENU GŁÓWNE"
    putStrLn "1 - Dodaj zadanie"
    putStrLn "2 - Usuń zadanie"
    putStrLn "3 - Przeglądaj zadania"
    putStrLn "4 - Pokaż czas programu"
    putStrLn "5 - Ustaw czas programu"
    putStrLn "6 - Zapisz do pliku"
    putStrLn "7 - Wczytaj z pliku"
    putStrLn "0 - Wyjdź"
    c <- getChar
    putStrLn ""
    case c of
        '1' -> do
            newTask <- makeTask
            menu (TaskBook date (newTask:tasks))
        '2' -> do
            menu (TaskBook date tasks)
        '3' -> do
--        1)wszystkie 2)zrealizowane 3)niezrealizowane dzisiejsze i zaległe
            putStrLn "Wyświetlanie zadań"
            print (TaskBook date tasks)
            task <- getChoiceNumber tasks
            putStrLn "wybrane zadanie:"
            print task
            menu (TaskBook date tasks)
        '4' -> do
            putStrLn ("Czas programu: " ++ show(date))
            menu (TaskBook date tasks)
        '5' -> do
            time <- getSafeDate
            putStrLn ("Czas programu zmieniony na: " ++ show(time))
            menu (TaskBook time tasks)
        '6' -> do
        		saveToFile tasks
        		menu (TaskBook date tasks)
        '7' -> do
        		newTasks <- readFromFile tasks
        		menu (TaskBook date newTasks)
        '0' -> putStrLn "Do widzenia!"
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            menu (TaskBook date tasks)

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
    choice <- getChar
    putStrLn ""
    case choice of
        '1' -> return NoRepeat
        '2' -> return EveryDay
        '3' -> return EveryWeek
        '4' -> return EveryMonth
        '5' -> return EveryYear
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            getRepeatability

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

getChoiceNumber tasks = do
    putStrLn "Podaj numer zadania:"
    choice <- getLine
    let choiceNr = read (choice)::Int
    if(choiceNr>=0 && choiceNr < (length tasks)) then return $ (!!) tasks choiceNr
        else getChoiceNumber tasks

addDay (UTCTime day time) = UTCTime (addDays 1 day) time
addWeek (UTCTime day time) = UTCTime (addDays 7 day) time
addMonth (UTCTime day time) = UTCTime (addDays 30 day) time
addYear (UTCTime day time) = UTCTime (addDays 365 day) time

saveToFile tasks = do
	putStrLn "Podaj nazwę pliku:"
	fileName <- getLine
	handle <- openFile fileName WriteMode
	saveTasks tasks handle
	hClose handle

saveTasks [] _ = do return()
saveTasks (x:xs) handle = do
	hPutStrLn handle (name x)
	hPutStrLn handle (show(time x))
	hPutStrLn handle (show(repeatability x))
	hPutStrLn handle (show(isCompleted x))
	saveTasks xs handle

readFromFile tasks = do
	putStrLn "Podaj nazwę pliku:"
	fileName <- getLine
	fileExists <- doesFileExist fileName
	doRead fileExists fileName tasks

doRead False _ tasks = do
	putStrLn "Plik nie istnieje!"
	return tasks

doRead True fileName tasks = do
	handle <- openFile fileName ReadMode
	content <- hGetContents handle
	let linesOfFile = lines content
	return $ parseTasks linesOfFile

parseTasks [] = []

parseTasks linesOfFile =
	let 
		taskLines = take 4 linesOfFile
		rest = drop 4 linesOfFile
		taskName = taskLines !! 0
		--date = UTCTime (fromGregorian 2011 12 16) (fromIntegral $ 12 * 3600)
		date = fromJust (parseDate (taskLines !! 1))
		rep = parseRep (taskLines !! 2)
		isComp = parseBool (taskLines !! 3)
		newTask = Task {name = taskName, time = date, repeatability = rep, isCompleted = isComp}
	in newTask:parseTasks rest
	
parseRep "Jednorazowe" = NoRepeat
parseRep "Codzienne" = EveryDay
parseRep "Cotygodniowe" = EveryWeek
parseRep "Comiesięczne" = EveryMonth
parseRep "Coroczne" = EveryYear

parseBool "True" = True
parseBool "False" = False

parseDate text =
	parseTime defaultTimeLocale "%Y-%m-%d %T UTC" text :: Maybe UTCTime


