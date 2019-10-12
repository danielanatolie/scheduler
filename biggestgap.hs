import Data.List
import Data.Maybe

-- Finding biggest gap (free time) in a list of start and end time tuples

-- Take merged sorted list and do the following below

-- Takes 2 tuples x and y. In order to find the free time between them,
-- subtract the start time of y with end time of x
findFreeTime :: (Int, Int) -> (Int, Int) -> Int

findFreeTime x y = (fst y) - (snd x)

{-- TEST CASES
	TEST1:
	input: findFreeTime (5,10) (14,16)
	output: 4

	TEST2:
	input: findFreeTime (5,9) (14,16)
	output: 5

	TEST3:
	input: findFreeTime (0,0) (14,16)
	output: 14

	TEST4:
	input: findFreeTime (5,10) (24,24)
	output: 14

	TEST5:
	input: findFreeTime (0,5) (14,16)
	output: 9

	TEST6:
	input: findFreeTime (0,0) (24,24)
	output: 24

-}

-- get a list of all hours that are free
listOfFreeTimes :: [(Int, Int)] -> [Int]
-- if no schedule, you have all day (24 hours)
listOfFreeTimes [] = [24]
-- finds the hours of free times when a class ends and the next one starts
listOfFreeTimes (a:b:c) = findFreeTime (0,0) a:findFreeTime a b: listOfFreeTimes2 (b:c)
 where
  listOfFreeTimes2 (a:b:c) = findFreeTime a b : listOfFreeTimes2 (b:c)
  listOfFreeTimes2 (a:b) = [findFreeTime a (24,24)]
  listOfFreeTimes2 lst = []
listOfFreeTimes (a:b) = [findFreeTime (0,0) a,findFreeTime a (24,24)]

{--
	TEST CASES:
	TEST1:
	input: listOfFreeTimes [(0,5),(10,14),(15,17)]
	output: [0,5,1,7]

	TEST2:
	input: listOfFreeTimes [] 
	output: [24]


	TEST3:
	input: listOfFreeTimes [(10,11)]
	output: [10,13]

-}

-- Find the max value in a list of free times
findMaxFreeTime :: [(Int, Int)] -> Int
findMaxFreeTime [] = 24
findMaxFreeTime x = maximum (listOfFreeTimes x)

{-- TEST CASES

	TEST1:
	input: findMaxFreeTime [(0,13),(15,24)]
	output: 2

	TEST2:
	input: findMaxFreeTime [(0,14)]
	output: 10

	TEST3:
	input: findMaxFreeTime [(9,11),(15,18),(20,21)]
	output: 9

-}

--Takes in the intervals and returns the 2 tuples (x and y) that produces the MaxFreeTime
findTuplesOfMaxValue :: [(Int, Int)] -> (Int, Int)
findTuplesOfMaxValue [] = (0,24)
findTuplesOfMaxValue x = (snd (y!!index), fst (y!!(index+1)))
 where y = [(0,0)] ++ x ++ [(24,24)]
       index = fromJust (findIndex (== (maximum (listOfFreeTimes x))) (listOfFreeTimes x))

{-- TEST CASES

	TEST1:
	input: findTuplesOfMaxValue [(9,11),(15,18),(20,21)]
	output: (0,9)

	TEST2:
	input: findTuplesOfMaxValue [(0,11),(15,18),(20,21)]
	output: (11,15)
-}


