import System.IO
import Data.Typeable
import Data.Char

main =
    do
        welcomePrinter
        fileName <- getLine
        putStrLn ("You've entered: "++fileName)
        handle <- openFile (fileName) ReadMode
        contents <- hGetContents handle
        let timeData = map strArrToTupleInt ([splitsep (==',') line | line <- splitsep (=='\n') contents])
        print timeData
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
    let res = funcChooser optionName
    print res
    putStrLn ("You have chosen option: "++optionName)

funcChooser c
    | c == "1" = "YESS"
    | otherwise = "NOPE"

strArrToTupleInt list = ((digitToInt (list!!0!!0)), (digitToInt (list!!1!!0)))

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t
