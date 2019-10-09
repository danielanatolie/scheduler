-- Finding biggest gap

-- create a new type interval which is a tuple of integers
type Interval = (Integer, Integer)

-- Take merged sorted list and do the following below

-- Takes 2 tuples x and y. In order to find the free time between them,
-- subtract the start time of y with end time of x

findFreeTime :: Interval -> Interval -> Integer
-- Base case is when you go to sleep MINUS end time of x
findFreeTime x _ = (sleep - snd x)
-- start time of y MINUS end time of x
findFreeTime x y = (fst y) - (snd x)

-- get a list of all hours that are free
listOfFreeTimes :: [Interval] -> [Integer]
-- if no schedule, you have all day from when you wake up and go to sleep
listOfFreeTimes [] = [sleep - wakeUp]
-- finds the hours of free times when a class ends and the next one starts
listOfFreeTimes (a:b:c) = 
	findFreeTime a b: listOfFreeTimes b c 

findMaxFreeTime :: [Interval] -> [Interval]
findMaxFreeTime [] = []
findMaxFreeTime x  = 
	-- find the index where we find the max free time from listoffreetimes
	let index = findIndex (== (max (listOfFreeTimes x))) (listOfFreeTimes x)
	-- goes back to the listoffreetimes and shows the tuple where we get the end time and the next tuple of the start time that 
	-- produce the biggest number by subtracting the start time of y and end time of x
	[x!!index, x!!(index+1)]

-- putStrLn $ "You have "++ (y starttime - x endtime)++ " hours between "++ (x endtime)++ " to " 
-- ++ (y starttime)



