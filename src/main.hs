import System.IO
import Data.Typeable
import Data.Char
import MergeIntervals
import BiggestGap
import ConflictDetector

--Entrance of the scheduler program, extracts time data from input .txt file and outputs data analysis to output.txt
main :: IO ()
main =
    do
        welcomePrinter
        fileName <- getLine
        handle <- openFile (fileName) ReadMode
        contents <- hGetContents handle
        let timeData = map strArrToTupleInt ([splitsep (==',') line | line <- splitsep (=='\n') contents])
        printOutput timeData

--Prints welcome message
welcomePrinter :: IO ()
welcomePrinter = do
    putStrLn "Welcome to scheduler!"
    putStrLn "Please enter the name of your .txt file that contains time data"

--Triggers final output to be printed and thank you message
printOutput :: [(Integer, Integer)] -> IO ()
printOutput timeData = do
    writeOuputToFile timeData
    putStrLn "Your data analysis has been saved to output.txt"
    putStrLn "Thank you for using scheduler!"

--Converts [String] to usuable Integer tuple
strArrToTupleInt :: [String] -> (Integer, Integer)
strArrToTupleInt list = ((read (list!!0) :: Integer), (read (list!!1) :: Integer))

--Extracts file lines given seperator, producing [[Char]]
--Credits to: https://www.cs.ubc.ca/~poole/cs312/2019/haskell/ReadCSV.hs
splitsep :: (a -> Bool) -> [a] -> [[a]]
splitsep sep [] = [[]]
splitsep sep (h:t)
    | sep h = []: splitsep sep t
    | otherwise = ((h:w):rest)
                where w:rest = splitsep sep t

--Gathers required data and outputs to output.txt
writeOuputToFile :: [(Integer, Integer)] -> IO ()
writeOuputToFile input = 
    writeFile "output.txt" outputText
    where 
    inputText = input
    maxFreeTime = findMaxFreeTime input
    maxFreeTimeTuples = findTuplesOfMaxValue input
    conflicts = conflictExists input
    mergedTime = checkAndMergeIntervals input 
    outputText = 
        "Hello! " ++ "\n" ++ "Here are your results: \n" 
        ++ "Original Data: " ++ (show inputText) ++ "\n" 
        ++ "Free Time: " ++(show maxFreeTime)  ++" hours between " ++ (show (fst maxFreeTimeTuples))++ " and "++(show (snd maxFreeTimeTuples))++ "\n"
        ++ "Conflicts: " ++ conflicts ++ "\n"
        ++ "Merged Interval Schedule: " ++ (show mergedTime) ++ "\n" 
        ++ "Thank you for using Scheduler! \n" 
