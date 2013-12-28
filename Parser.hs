module Parser
(
	parseTasks
) where

import Logic
import System.Locale
import Data.Time
import Data.Time.Format
import Data.Maybe

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

parseDate text =
	parseTime defaultTimeLocale "%Y-%m-%d %T UTC" text :: Maybe UTCTime


