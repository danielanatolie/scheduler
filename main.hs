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
        optionPrinter timeData

welcomePrinter = do
    putStrLn "Welcome to scheduler!"
    putStrLn "Please enter the name of your .csv file that contains time data"

optionPrinter timeData = do
    putStrLn "Please choose one of the following options:"
    putStrLn "1 : can entity exist in all time intervals?"
    putStrLn "2 : get biggest gap within intervals."
    putStrLn "3 : merge overlapping intervals."
    optionName <- getLine
    let res = funcChooser optionName timeData
    print res
    putStrLn ("You have chosen option: "++optionName)

funcChooser option timeData
    | option == "1" = canAttendAll timeData
    | otherwise = "No such option available."

canAttendAll [time] = "True"
canAttendAll (time1:time2:timeData)
    | overlap time1 time2 = "False"
    | otherwise = canAttendAll (time2:timeData)

overlap time1 time2 = (snd (time1)) > (fst (time2))

strArrToTupleInt list = ((digitToInt (list!!0!!0)), (digitToInt (list!!1!!0)))

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t
