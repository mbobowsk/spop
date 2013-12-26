main = do
	putStrLn "Witamy w systemie przypomnień"
	menu []
  
menu prompts = do
	putStrLn "Wybierz jedną z akcji:"
	putStrLn "(D)odaj    (P)rzeglądaj"
	putStrLn "(W)yjście"
	action <- getChar
	putStrLn ""
	case action of
		'd' -> do
							a <- doAdd' prompts
							-- menu(doAdd' prompts)
							menu a
		'p' -> do 
			doBrowse prompts
			menu prompts
		'w' -> putStrLn "Have a nice day!"
		_  -> menu prompts

doAdd prompts = "a":prompts


doAdd' prompts = do
	putStrLn "Podaj nazwę:"
	name <- getLine
	return (name:prompts)

doBrowse prompts = do print prompts

