import Data.List
import Data.Maybe
-- Finding biggest gap

-- create a new type interval which is a tuple of integers
type Interval = (Integer, Integer)

-- Take merged sorted list and do the following below

-- Takes 2 tuples x and y. In order to find the free time between them,
-- subtract the start time of y with end time of x

findFreeTime :: Interval -> Interval -> Integer
-- Base case is when you go to sleep MINUS end time of x
-- start time of y MINUS end time of x
findFreeTime x y = (fst y) - (snd x)

-- get a list of all hours that are free
listOfFreeTimes :: [Interval] -> [Integer]
-- if no schedule, you have all day from when you wake up and go to sleep
listOfFreeTimes [] = [24]
-- finds the hours of free times when a class ends and the next one starts
listOfFreeTimes (a:b:c) = findFreeTime (0,0) a:findFreeTime a b: listOfFreeTimes2 (b:c)
 where
  listOfFreeTimes2 (a:b:c) = findFreeTime a b : listOfFreeTimes2 (b:c)
  listOfFreeTimes2 (a:b) = [findFreeTime a (24,24)]
  listOfFreeTimes2 lst = []
listOfFreeTimes (a:b) = [findFreeTime a (24,24)]

--max::[Integer]->Integer
--max [a] = a
--max (a:b:c)
-- | a > b = max (a:c)
-- | otherwise = max (b:c)

--findIndex :: (a -> Bool) -> [a] -> Int
-- findIndex x (h:t)



findMaxFreeTime :: [Interval] -> [Interval]
findMaxFreeTime [] = []
findMaxFreeTime x  
 | length x == 1 = x
-- | length x == 2 = 
 | otherwise =
 -- find the index where we find the max free time from listoffreetimes


-- gives the index of the max value in listoffreetimes on the originial merged and sorted input given
-- which we then take
 [x!!index]
 where
  index = fromJust (findIndex (== (maximum (listOfFreeTimes x))) (listOfFreeTimes x))


-- putStrLn $ "You have "++ (max (listOfFreeTimes x) ++ " hours between "++ (x endtime)++ " to " 
-- ++ (y starttime)

{-- TEST CASES

	Test Case 1:
	input:
		[(0,13),(15,24)]
	output:
		2


	Test Case 2:
	input:
		[(0,14)]
	output:
		10

	Test Case 1:
	input:
		[(9,11),(15,18),(20,21)]
	output:
		9


--}
