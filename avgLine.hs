line func = map func [1..]

f x = x*x
xy x = (x, (f x))
myTake = take 10
fLine = line xy
d = myTake fLine   

--justYs :: [(a,b)] -> b
ysum = sum (map (\(x,y)->y) d)
avg = (fromIntegral ysum) / (fromIntegral(length d))
avgF _ = avg
avgxy x = (x, avgF x)
dLine = line avgxy
avgD = myTake dLine

main = do
       print d
       print avgD
