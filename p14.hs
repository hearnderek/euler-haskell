import Data.List (foldl')
-- The following iterative sequence is defined for the set of positive integers:
--   n -> n/2 (n is even)
--   n -> 3n + 1 (n is odd)
--
-- Using the rule above and starting with 13, we generate the following sequence:
--   13-40-20-10-5-16-8-4-2-1
--
-- It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms.
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
-- Which starting number, under one million, produces the longest chain?

-- StartingNumber -> [seq] -> Length
collatz :: Integer -> [Integer]
collatz 1 = [1]
collatz n = n : collatz (next n)
  where 
  next x | even x = div x 2
         | otherwise = 3*x + 1
  
inout :: (a -> b) -> a -> (a, b)
inout f a = (a, f a) 

tmax :: Ord v => (k,v) -> (k,v) -> (k,v)
tmax a b | (snd a) >= (snd b) = a
         | otherwise          = b

startAndSeqLen :: Integer -> (Integer, Int)
startAndSeqLen = inout (length . collatz)

lastCollatz :: Integer -> Integer
lastCollatz x = if odd od then od else ev 
  where 
  od = div (x - 1) 3
  ev = x * 2

maxStart :: Integer
maxStart = 999999

minStart :: Integer
minStart = od + (mod (od + 1) 2)
  where od = div (maxStart - 1) 3 

possibles :: [Integer]
possibles = filter ((maxStart <) . lastCollatz) [minStart,minStart+2..maxStart]

-- I need fold' for seq. Otherwise we get a stack-overflow exception
-- we can safely ignore the even numbers for the sake of speed
finalFaster :: (Integer, Int)
finalFaster = foldl' tmax (1,1) (map startAndSeqLen possibles)

finalStart :: Integer
finalStart = fst finalFaster

finalSeq :: [Integer]
finalSeq = collatz finalStart

main :: IO ()
main = do
  print (finalSeq)
  print (finalFaster)
