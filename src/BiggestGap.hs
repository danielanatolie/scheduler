module BiggestGap where
import Data.List
import Data.Maybe

-- Finding biggest gap (free time) in a list of start and end time tuples
-- Takes 2 tuples x and y. In order to find the free time between them,
-- subtract the start time of y with end time of x
findFreeTime :: (Integer, Integer) -> (Integer, Integer) -> Integer
findFreeTime x y = (fst y) - (snd x)

-- get a list of all hours that are free
listOfFreeTimes :: [(Integer, Integer)] -> [Integer]
-- if no schedule, you have all day (24 hours)
listOfFreeTimes [] = [24]
-- finds the hours of free times when a class ends and the next one starts
listOfFreeTimes (a:b:c) = findFreeTime (0,0) a:findFreeTime a b: listOfFreeTimes2 (b:c)
 where
  listOfFreeTimes2 (a:b:c) = findFreeTime a b : listOfFreeTimes2 (b:c)
  listOfFreeTimes2 (a:b) = [findFreeTime a (24,24)]
  listOfFreeTimes2 lst = []
listOfFreeTimes (a:b) = [findFreeTime (0,0) a,findFreeTime a (24,24)]

-- Find the max value in a list of free times
findMaxFreeTime :: [(Integer, Integer)] -> Integer
findMaxFreeTime [] = 24
findMaxFreeTime x = maximum (listOfFreeTimes x)

--Takes in the intervals and returns the 2 tuples (x and y) that produces the MaxFreeTime
findTuplesOfMaxValue :: [(Integer, Integer)] -> (Integer, Integer)
findTuplesOfMaxValue [] = (0,24)
findTuplesOfMaxValue x = (snd (y!!index), fst (y!!(index+1)))
 where y = [(0,0)] ++ x ++ [(24,24)]
       index = fromJust (findIndex (== (maximum (listOfFreeTimes x))) (listOfFreeTimes x))
