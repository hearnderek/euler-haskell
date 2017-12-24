-- If we list all the natural numbers below 10 that are multiples of 3 or 5, 
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.

-- Find the sum of all the multiples of 3 or 5 below 1000.

-- Generates all the numbers I need
mulOf x = [x,x*2..]

-- takes two sorted sets and combined them into one sorted set
myZip (x:xs) (y:ys) | x < y  = x : (myZip xs     (y:ys))
                    | x > y  = y : (myZip (x:xs) ys) 
		    -- We don't want duplicate values
                    | x == y = x : (myZip xs     ys) 
myZip []     (y:ys) = y : ys
myZip (x:xs) []     = x : xs
myZip []     []     = []

mulOf3n5under1k = takeWhile (<1000) (myZip (mulOf 3) (mulOf 5))

main = putStrLn ( show ( sum mulOf3n5under1k)) 
