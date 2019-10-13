import System.IO
import Data.Typeable
import Data.Char
import MergeIntervals
import BiggestGap

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
    putStrLn "Please enter the name of your .txt file that contains time data"

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
    | option == "2" = findTuplesOfMaxValue mergedList && writeOutputToGapFile timeData
    | option == "3" = mergedList && writeOuputToFile timeData
    | otherwise = "No such option available."
      where mergedList = checkAndMergeIntervals timeData

canAttendAll [time] = "True"
canAttendAll (time1:time2:timeData)
    | overlap time1 time2 = "False"
    | otherwise = canAttendAll (time2:timeData)

overlap time1 time2 = (snd (time1)) > (fst (time2))

strArrToTupleInt list = ((read (list!!0) :: Integer), (read (list!!1) :: Integer))

splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t