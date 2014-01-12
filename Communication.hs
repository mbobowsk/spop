module Communication
( menu,
  makeTask
) where

import Logic
import Parser

import System.Locale
import Data.Time
import Data.Time.Format
import Data.Maybe
import System.IO
import System.Directory
import Data.Char

menu (TaskBook date tasks) = do
    putStrLn "\nMENU GŁÓWNE"
    putStrLn "1) - Dodaj zadanie"
    putStrLn "2) - Przeglądaj zadania"
    putStrLn "3) - Pokaż czas programu"
    putStrLn "4) - Ustaw czas programu"
    putStrLn "5) - Zapisz do pliku"
    putStrLn "6) - Wczytaj z pliku"
    putStrLn "0) - Wyjdź"
    c <- getLine
    case (getNumberFromLine c) of
        1 -> do
            newTask <- makeTask
            menu (TaskBook date (newTask:tasks))
        2 -> do
            tasksMenu (TaskBook date tasks)
        3 -> do
            putStrLn ("Czas programu: " ++ show(date))
            menu (TaskBook date tasks)
        4 -> do
            time <- getSafeDate
            putStrLn ("Czas programu zmieniony na: " ++ show(time))
            menu (TaskBook time tasks)
        5 -> do
        		saveToFile tasks
        		menu (TaskBook date tasks)
        6 -> do
        		newTasks <- readFromFile tasks
        		menu (TaskBook date newTasks)
        0 -> putStrLn "Do widzenia!"
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            menu (TaskBook date tasks)

getNumberFromLine line =
	let
		lengthOk = length line > 0
		digitOk = isDigit (line !! 0)
		digit = substr line 0 0
	in
		if (lengthOk && digitOk)
			then read digit :: Int
			else 999 -- magic number

makeTask = do
    putStrLn "Podaj nazwe zadania:"
    taskName <- getLine
    rep <- getRepeatability
    date <- getSafeDate
    return $ Task {name = taskName, time = date, repeatability = rep, isCompleted = False}

-- gets from user repeatability of task
getRepeatability = do
    putStrLn "Powtarzalność zadania:"
    putStrLn "1) - Jednorazowe"
    putStrLn "2) - Codziennie"
    putStrLn "3) - Co tydzień"
    putStrLn "4) - Co miesiąc"
    putStrLn "5) - Co rok"
    choice <- getLine
    case (getNumberFromLine choice) of
        1 -> return NoRepeat
        2 -> return EveryDay
        3 -> return EveryWeek
        4 -> return EveryMonth
        5 -> return EveryYear
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            getRepeatability

-- Get date until it is correct
-- Returns UTCTime
getSafeDate = do
	date <- getDate
	if (isNothing date)
		then	getSafeDate
		else	return (fromJust date)

-- Get date from user and parse it
-- Returns Maybe UTCTime
getDate = do
	putStrLn "Podaj datę w formacie yyyy-mm-dd HH:MM:"
	dateString <- getLine
	let
		parsedTime = parseTime defaultTimeLocale "%Y-%m-%d %H:%M" dateString :: Maybe UTCTime
		parseable = preparse dateString
	returnDate parseable parsedTime

-- Wrapper for return function
returnDate False _ = return Nothing
returnDate True parsedTime = return parsedTime

tasksMenu (TaskBook date tasks) = do
    putStrLn "Wybierz zadania do wyświetlenia:"
    putStrLn "1) - Wszystkie"
    putStrLn "2) - Zrealizowane"
    putStrLn "3) - Do zrobienia dzisiaj i zaległe"
    putStrLn "0) - powrót do menu"
    choice <- getLine
    case (read choice::Int) of
        0 -> do
            menu (TaskBook date tasks)
        1 -> do
            print (TaskBook date tasks)
            task <- chooseTask tasks
            if (isNothing task)
                then	menu (TaskBook date tasks)
                else	do
                    putStrLn "wybrane zadanie:"
                    print $ fromJust task
                    modifyTask (TaskBook date tasks) (fromJust task)
        2 -> do
            let completedTasks = filter (\(Task _ _ _ isCompleted) -> isCompleted) tasks
            print (TaskBook date completedTasks)
            task <- chooseTask completedTasks
            if (isNothing task)
                then	menu (TaskBook date tasks)
                else	do
                    putStrLn "wybrane zadanie:"
                    print $ fromJust task
                    modifyTask (TaskBook date tasks) (fromJust task)
        3 -> do
            let (UTCTime today time) = date
            let todoTasks = filter (isTodoTask today) tasks
            print (TaskBook date todoTasks)
            task <- chooseTask todoTasks
            if (isNothing task)
                then	menu (TaskBook date tasks)
                else	do
                    putStrLn "wybrane zadanie:"
                    print $ fromJust task
                    modifyTask (TaskBook date tasks) (fromJust task)
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            tasksMenu (TaskBook date tasks)

chooseTask tasks = do
    putStrLn "Wybierz zadanie lub wróć do menu(0):"
    choice <- getLine
    let choiceNr = read (choice)::Int
    if(choiceNr == 0) then return Nothing
    	else if(choiceNr>0 && choiceNr < (1+length tasks)) then return $ Just  (tasks !! (length tasks - choiceNr))
    		else chooseTask tasks

modifyTask (TaskBook date tasks) task = do
    putStrLn "Wybierz opcję:"
    putStrLn "1 - Usuń zadanie"
    putStrLn "2 - Oznacz zadanie jako wykonane"
    putStrLn "0 - Wróć"
    choice <- getLine
    case (getNumberFromLine choice) of
        0 -> do
            putStrLn "Powrót do menu"
            menu (TaskBook date tasks)
        1 -> do
            putStrLn "usuwanie zadania"
            deleteTask (TaskBook date tasks) task
        2 -> do
            putStrLn "oznaczanie zadania"
            markTaskAsCompleted (TaskBook date tasks) task
        _ -> do
            putStrLn "Nieprawidłowy wybór"
            modifyTask (TaskBook date tasks) task


deleteTask  (TaskBook date tasks) task = do
   let newTasks = filter (/=task) tasks
   putStrLn "Task deleted"
   menu (TaskBook date newTasks)

isTodoTask today (Task _ (UTCTime day time) _ isCompleted) =
    if(not isCompleted && (day <= today)) then True
    else False

markTaskAsCompleted (TaskBook date tasks) task = do
    let newTasks = map (\t -> if t==task then setTaskCompleted t else t) tasks
    let (Task name time repeatability isCompleted) = task
    if repeatability /= NoRepeat then
        case repeatability of
        EveryDay -> do
            let newTask = (Task name (addDay time) repeatability False)
            menu (TaskBook date (newTask:newTasks))
        EveryWeek -> do
            let newTask = (Task name (addWeek time) repeatability False)
            menu (TaskBook date (newTask:newTasks))
        EveryMonth -> do
            let newTask = (Task name (addMonth time) repeatability False)
            menu (TaskBook date (newTask:newTasks))
        EveryYear -> do
            let newTask = (Task name (addYear time) repeatability False)
            menu (TaskBook date (newTask:newTasks))
        else
            menu (TaskBook date newTasks)

setTaskCompleted (Task name time repeatability _) = (Task name time repeatability True)

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

