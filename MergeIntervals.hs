module MergeIntervals where
import Data.List

-- Given the input (a list of tuples), check if there is any overlap, if so merge the tuples into one ---


-- Function that is comparing two tuples and checking if they overlap, returns True if tuples overlap, False otherwise 
-- assumption, that our tuples are ordered so the tuple that comes first (in 24 hour clock) will always be the function's first input 
checkOverlap :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
checkOverlap (0,0) _ = False
checkOverlap _ (0,0) = False
checkOverlap (a,b) (c,d) = 
    if b >= c || b >= d then True else False 

-- TESTS:
-- checkOverlap (1,2) (2,5); Expecting True; Result is True
-- checkOverlap (1,2) (3,5); Expecting False; Result is False 
-- checkOverlap (1,6) (2,9); Expecting True; Result is True
-- checkOverlap (1,6) (2,5); Expecting True; Result is True 
-- checkOverlap (2,5) (2,5); Expecting True; Result is True 

-- Function that combines/merges two tuples into one, returns the correct tuple with min of the two start times 
-- and max of the two end times 
merge2Intervals :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
merge2Intervals (a,b) (c,d) = (min a c, max b d)

-- TESTS:
-- merge (1,2) (2,5); Expecting (1,5); Result is (1,5)
-- merge (1,2) (3,5); Expecting (1,5); Result is (1,5) 
-- merge (1,6) (2,9); Expecting (1,9); Result is (1,9)
-- merge (1,6) (2,5); Expecting (1,6); Result is (1,6) 
-- merge (2,5) (2,5); Expecting (2,5); Result is (2,5) 

-- Function that sorts the given list of tuples only by first element of tuple
-- basically checking the first value in the tuple and comparing it to the next 
sortIntervals:: [(Integer, Integer)] -> [(Integer, Integer)]
sortIntervals = sortBy (\ (a,_) (c,_) -> (compare a c))

-- TESTS:
-- sortIntervals [(3,6), (1,3), (9,16)]; Expecting [(1,3), (3,6), (9,16)]; Result is [(1,3), (3,6), (9,16)]
-- sortIntervals [(3,6), (1,3), (1,2)]; Expecting [(1,3), (1,2), (3,6)]; Result is [(1,3), (1,2), (3,6)]
-- sortIntervals [(1,2), (3,4), (5,16)]; Expecting [(1,2), (3,4), (5,16)]; Result is [(1,2), (3,4), (5,16)]

-- Function that is going through sorted list of tuples, merging any tuples that are overlapping 
-- and then appending it to a list to get a final sorted and merged tuple list 
checkAndMergeIntervals :: [(Integer, Integer)] -> [(Integer, Integer)]
checkAndMergeIntervals [] = []
checkAndMergeIntervals interval = 
    foldr mapThroughList [] (sortIntervals interval)
    where mapThroughList oneInterval [] = [oneInterval]
          mapThroughList x (h:t) = 
            if checkOverlap x h then [(merge2Intervals x h)]++t else [x]++(h:t)

-- TESTS:
-- checkAndMergeIntervals [(3,6), (1,3), (9,16)]; Expecting [(1,6), (9,16)]; Result is [(1,6), (9,16)]
-- checkAndMergeIntervals [(3,6), (1,3), (5,16)]; Expecting [(1,16)]; Result is [(1,16)]
-- checkAndMergeIntervals [(6,10), (7,9), (4,5), (1,4), (7,9)]; Expecting [(1,5), (6,10)]; Result is [(1,5), (6,10)]
    
-- Function that takes the original list of tuples, applies the checkAndMergeIntervals function to get the final sorted and merged tuple list
-- and then writes to output file a message that outlines thier orginal input vs final output 
writeOuputToFile :: [(Integer, Integer)] -> IO ()
writeOuputToFile input = 
    writeFile "mergeOutput1.txt" outputText
 where 
    inputText = input
    funcOutput = checkAndMergeIntervals input 
    outputText = "Hello! Here are your results from using Merge Intervals: \n" ++ "input: " ++ (show inputText) ++ "\n" 
     ++ "output: " ++ (show funcOutput) ++ "\n" ++ "Thanks for using Scheduler! \n" 
     
-- writeOuputToFile [(3,6), (1,3), (9,16)]; Expecting correct message with output = [(1,6), (9,16)]; Result is correct message within file
-- writeOuputToFile [(7,9), (3,5), (7,9)]; Expecting correct message with output = [(3,5),(7,9)]; Result is correct message within file 