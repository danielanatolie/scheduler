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

-- Function that combines/merges two tuples into one, returns the correct tuple with min of the two start times 
-- and max of the two end times 
merge2Intervals :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
merge2Intervals (a,b) (c,d) = (min a c, max b d)

-- Function that sorts the given list of tuples only by first element of tuple
-- basically checking the first value in the tuple and comparing it to the next 
sortIntervals:: [(Integer, Integer)] -> [(Integer, Integer)]
sortIntervals = sortBy (\ (a,_) (c,_) -> (compare a c))

-- Function that is going through sorted list of tuples, merging any tuples that are overlapping 
-- and then appending it to a list to get a final sorted and merged tuple list 
checkAndMergeIntervals :: [(Integer, Integer)] -> [(Integer, Integer)]
checkAndMergeIntervals [] = []
checkAndMergeIntervals interval = 
    foldr mapThroughList [] (sortIntervals interval)
    where mapThroughList oneInterval [] = [oneInterval]
          mapThroughList x (h:t) = 
            if checkOverlap x h then [(merge2Intervals x h)]++t else [x]++(h:t)
