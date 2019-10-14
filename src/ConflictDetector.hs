module ConflictDetector where

--Detects whether conflicts exist within data interval data
conflictExists [time] = "No conflicts detected."
conflictExists (time1:time2:timeData)
    | overlap time1 time2 = "A conflict occurs between "++(show time1)++ " and "++(show time2) ++ "."
    | otherwise = conflictExists (time2:timeData)

overlap time1 time2 = (snd (time1)) > (fst (time2))
