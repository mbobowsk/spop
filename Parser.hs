module Parser
(
	parseTasks,
	preparse
) where

import Logic
import System.Locale
import Data.Time
import Data.Time.Format
import Data.Maybe
import Data.Char

-- Parse tasks from lines of file
-- Returns list of Tasks
parseTasks [] = []

parseTasks linesOfFile =
	let 
		taskLines = take 4 linesOfFile
		rest = drop 4 linesOfFile
		newTask = parseTask taskLines
	in if (isNothing newTask)
			then parseTasks rest
			else (fromJust newTask):parseTasks rest

-- Parse single task from 4 consecutive lines of file
-- Returns task
parseTask taskLines =
	let
		taskName = taskLines !! 0
		date = parseDate (taskLines !! 1)
		rep = parseRep (taskLines !! 2)
		isComp = parseBool (taskLines !! 3)
	in if (isNothing date || isNothing rep || isNothing isComp)
			then Nothing
			else Just Task {name = taskName, time = fromJust date, repeatability = fromJust rep, isCompleted = fromJust isComp}

-- Safely parse task elements
parseRep "Jednorazowe" = Just NoRepeat
parseRep "Codzienne" = Just EveryDay
parseRep "Cotygodniowe" = Just EveryWeek
parseRep "Comiesięczne" = Just EveryMonth
parseRep "Coroczne" = Just EveryYear
parseRep _ = Nothing

parseBool "True" = Just True
parseBool "False" = Just False
parseBool _ = Nothing

parseDate text = parseTime defaultTimeLocale "%Y-%m-%d %T UTC" text :: Maybe UTCTime

-- Parse date string before it can be parsed with 'parseTime'
-- Returns True if pre-parse is successfull
preparse dateString =
	let
		month = substr dateString 5 6
		day = substr dateString 8 9
		hour = substr dateString 11 12
		minutes = substr dateString 14 15
	in
		(length dateString > 14) && preparseMonth month && preparseDay day && preparseHour hour && preparseMinutes minutes

-- Month must be an Int greater than 0 and less than 13
preparseMonth month = 
	let
		digits = (isDigit (month !! 0) && isDigit (month !! 1))
		value = read month :: Int
		valueOk = value > 0 && value < 13
	in
		digits && valueOk

-- Day must be an Int greater than 0 and less than 32
preparseDay day =
	let
		digits = (isDigit (day !! 0) && isDigit (day !! 1))
		value = read day :: Int
		valueOk = value > 0 && value < 32
	in
		digits && valueOk

-- Hour must be an Int greater than -1 and less than 25
preparseHour hour =
	let
		digits = (isDigit (hour !! 0) && isDigit (hour !! 1))
		value = read hour :: Int
		valueOk = value > -1 && value < 25
	in
		digits && valueOk

-- Minute must be an Int greater than -1 and less than 61
preparseMinutes minutes =
	let
		digits = (isDigit (minutes !! 0) && isDigit (minutes !! 1))
		value = read minutes :: Int
		valueOk = value > -1 && value < 61
	in
		digits && valueOk

-- Substring function
substr str begin end = drop begin $ take (end + 1) str
