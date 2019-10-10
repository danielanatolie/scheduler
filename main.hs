main =
    do
        welcomePrinter
        fileName <- getLine
        putStrLn ("You've entered: "++fileName)
        optionPrinter

welcomePrinter = do
    putStrLn "Welcome to scheduler!"
    putStrLn "Please enter the name of your .csv file that contains time data"

optionPrinter = do
    putStrLn "Please choose one of the following options:"
    putStrLn "1 : can entity exist in all time intervals?"
    putStrLn "2 : get biggest gap within intervals."
    putStrLn "3 : merge overlapping intervals."
    optionName <- getLine
    putStrLn ("You have chosen option: "++optionName)
