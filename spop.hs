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
	putStrLn "(T)est (W)yjście"
	action <- getChar
	putStrLn ""
	case action of
		'd' -> do
			a <- doAdd prompts
			menu a
		'p' -> do 
			doBrowse prompts
			menu prompts
		'w' -> putStrLn "Have a nice day!"
		't' -> do
			doTest
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

doBrowse prompts = do print prompts

doTest = do
	putStrLn "test"
