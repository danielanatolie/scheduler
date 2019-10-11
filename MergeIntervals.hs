import Data.List
-- Given the input (a list of tuples), check if there is any overlap, if so merge the tuples into one ---

-- create a new type interval which is a tuple of integers
type Interval = (Integer, Integer)

-- Function that is comparing two intervals and checking if they overlap, returns True if intervals overlap, False otherwise 
-- assumption, that our interval are ordered so the interval that occurs first (in 24 hour clock) will always be the first interval input
checkOverlap :: Interval -> Interval -> Bool
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

-- Function that combines/merges two intervals into one, returns the correct interval with min of the two start times 
-- and max of the two end times 
merge2Intervals :: Interval -> Interval -> Interval
merge2Intervals (a,b) (c,d) = (min a c, max b d)

-- TESTS:
-- merge (1,2) (2,5); Expecting (1,5); Result is (1,5)
-- merge (1,2) (3,5); Expecting (1,5); Result is (1,5) 
-- merge (1,6) (2,9); Expecting (1,9); Result is (1,9)
-- merge (1,6) (2,5); Expecting (1,6); Result is (1,6) 
-- merge (2,5) (2,5); Expecting (2,5); Result is (2,5) 

-- Function that sorts the given list of intervals only by first element of tuple
-- basically checking the first value in the tuple and comparing it to the next 
sortIntervals:: [Interval] -> [Interval]
sortIntervals = sortBy (\ (a,_) (c,_) -> (compare a c))

-- TESTS:
-- sortIntervals [(3,6), (1,3), (9,16)]; Expecting [(1,3), (3,6), (9,16)]; Result is [(1,3), (3,6), (9,16)]
-- sortIntervals [(3,6), (1,3), (1,2)]; Expecting [(1,3), (1,2), (3,6)]; Result is [(1,3), (1,2), (3,6)]
-- sortIntervals [(1,2), (3,4), (5,16)]; Expecting [(1,2), (3,4), (5,16)]; Result is [(1,2), (3,4), (5,16)]

-- Function that is going through sorted list of intervals, merging any intervals that are overlapping 
-- and then appending it to a list to get a final sorted and merged interval list 
checkAndMergeIntervals :: [(Interval)] -> [(Interval)]
checkAndMergeIntervals [] = []
checkAndMergeIntervals interval = 
    foldr mapThroughList [] (sortIntervals interval)
    where mapThroughList oneInterval [] = [oneInterval]
          mapThroughList x (h:t) = 
            if checkOverlap x h then [(merge2Intervals x h)]++t else [x]++(h:t)

-- TESTS:
-- checkAndMergeIntervals [(3,6), (1,3), (9,16)]; Expecting [(1,6), (9,16)]; Result is [(1,6), (9,16)]

    
--TODO: TEST BASE CASES and check for exceptional cases 
