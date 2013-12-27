import System.Locale
import Data.Time
import Data.Time.Format
import Data.Maybe

main = do
	putStrLn "Witamy w systemie przypomnień"
	menu []
  
menu prompts = do
	putStrLn "Wybierz jedną z akcji:"
	putStrLn "(D)odaj    (P)rzeglądaj"
	putStrLn "(A)ktualny czas"
	putStrLn "(T)est     (W)yjście"
	action <- getChar
	putStrLn ""
	case action of
		'd' -> do
			a <- doAdd prompts
			menu a
		'p' -> do 
			doBrowse prompts
			menu prompts
		'a' -> do
			printTimeNow
			menu prompts
		'w' -> putStrLn "Have a nice day!"
		't' -> do
			doTest prompts
			menu prompts
		_  -> menu prompts

doAdd prompts = do
	name <- getName
	date <- getSafeDate
	let task = (name, date)
	return (task:prompts)

getName = do
	putStrLn "Podaj nazwę zadania:"
	name <- getLine
	return name

printTimeNow = fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%M") getCurrentTime

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

doBrowse prompts = do print prompts

doTest prompts = do
	putStrLn "test"
