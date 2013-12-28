import System.Locale
import Data.Time
import Data.Maybe

import Communication
import Logic

main = do
    putStrLn "Hello, welcome to task manager!"
    date <- Data.Time.getCurrentTime
    menu (TaskBook date [])

