import Data.List (foldl')
-- will need to test out how 1 digit numbers work
-- otherwise decreasing 

--bouncy :: [Int] -> bool
--bouncy = 

digits :: Integer -> [Int]
digits = map (read . (:[])) . show

bouncyPairs :: [Int] -> [(Int,Int)]
bouncyPairs xs = zip xs (tail xs) 

isBouncy :: Integer -> Bool
isBouncy = fromDigits . digits

fromDigits :: [Int] -> Bool
fromDigits (d0:d1:d2:dr) 
  | d0 == d1             = fromDigits (d1:d2:dr)
  | d1 == d2             = fromDigits (d0:d2:dr)
  | d0 > d1 && d1 > d2   = fromDigits (d2:dr)
  | d0 < d1 && d1 < d2   = fromDigits (d2:dr)
  | d0 > d1 && d1 < d2   = True
  | d0 < d1 && d1 > d2   = True
  | otherwise            = False
fromDigits _             = False

threeStateBool :: Int -> Int -> Int
threeStateBool x y 
  | x > y = 1
  | x < y = -1
  | otherwise = 0

fromPairs :: [(Int,Int)] -> [Int]
fromPairs ((x,y):ps) = (threeStateBool x y):(fromPairs ps)
fromPairs [] = []

allPositive = all (>=0)
allNegative = all (<=0)

positive = allNegative
negative = allPositive
-- this is super slow.
bouncy x = not (positive x || negative x) 

-- can be reduced down to just bouncy or not
bnum x 
  | bouncy x = 2
  | otherwise  = 0

stats :: [Int] -> (Int, Int)
stats xs = ((length xs),length(filter (==2) xs))

bouncyPercent :: Fractional a => (Int, Int) -> a
bouncyPercent (x, y) = (fromIntegral y) / (fromIntegral (x))

toStats :: [Integer] -> (Int, Int)
toStats xs = stats $ map (bnum . fromPairs . bouncyPairs . digits) xs

foo :: [Integer] -> Integer
foo a@(x:_) 
  | ((bouncyPercent . toStats) a) >= 0.6 = x
  | otherwise = foo (x+1:a) 

stats2Fold :: (Fractional a, Ord a) => a -> Integer -> Integer -> (Integer, Integer)
stats2Fold limit c n 
  | (fromIntegral nextC) / (fromIntegral (n)) >= limit = (nextC, n)
  | otherwise = stats2Fold limit nextC (n+1)
  where nextC = if (isBouncy n) then c+1 else c


stats2Fold2 :: (Fractional a, Ord a) => a -> Integer -> Integer -> (Integer, Integer)
stats2Fold2 limit c n 
  | (fromIntegral nextC) / (fromIntegral (n)) >= limit = (nextC, n)
  | otherwise = stats2Fold limit nextC (n+1)
  where nextC = if (isBouncy n) then c+1 else c

add1WhenBouncy n = if (isBouncy n) then n+1 else n 

nextF :: (Fractional a, Ord a) => a -> (Integer, Integer) -> Integer
nextF limit (a,b)
  | x/y < limit = nextF limit (a+1,b+1)
  | otherwise = b
  where x = fromIntegral a
        y = fromIntegral b

nextG :: (Fractional a, Ord a) => a -> (Integer, Integer) -> Integer
nextG limit (a,b)
  | mod a 99 > 0 = nextG limit (a+m,b+m)
  | x/y < limit = nextG limit (a,b+1)
  | otherwise = b
  where x = fromIntegral a
        y = fromIntegral b
	m = a+(99-(mod a 99))
numBouncy :: Integer -> Integer -> Integer
numBouncy start end = foldl' (\x y->if isBouncy y then x+1 else x) 0 [start..end] 

bar :: (Integer, Integer) -> (Integer, Integer)
bar t@(a,b) = (a + (numBouncy b nf), nf)
  where nf = nextF 0.99 t
        ng = nextG 0.99 t
	nn = ng -- if ng < nf then ng else nf


bar2 :: (Integer, Integer) -> IO Integer
bar2 t@(a,b) 
  | x/y >= 0.99 = return b
  | otherwise = do 
    print $ "t: " ++ show t
    print $ "nn: " ++ show nn
    print $ "nb: " ++ show nb
    bar2 (a + nb, nn)
  where x = fromIntegral a
        y = fromIntegral b
	nb = numBouncy b nn
	nf = nextF 0.99 t
        ng = nextG 0.99 t
	nn = ng --if ng < nf then ng else nf

-- all bouncy numbers will stay bouncy regardless of how many numbers
--  are added to it.

-- once we figure out all values of xxx we can save the non bouncy numbers
-- [0..9] all non bouncy
-- for all non bouncy ^ add [0..9] in left most space
-- for all non bouncy ^ add [0..9] in left most space
toTenPower :: Integer -> Integer -> Integer
toTenPower a b = a*(10^b)

toTenPowerList a = [toTenPower 1 a, toTenPower 2 a..toTenPower 9 a]

toTenPowerListLast a = (toTenPower 1 (a+1))-1

fstlvl :: ([Integer], Integer, Integer)
fstlvl = (nonBouncy, nonBouncyCount, 0)
  where 
    l = [1..9]
    nonBouncy = filter (not . isBouncy) l
    nonBouncyCount = toInteger (length nonBouncy)

sndlvl = nlvl fstlvl

nlvl :: ([Integer], Integer, Integer) -> ([Integer], Integer, Integer)
nlvl (a,b,c) = (fullBouncy, fullBouncyCount, c+1)
  where
    l = [x+y|x <- toTenPowerList (c+1), y <-((0*(10^c)):a)]
    nonBouncy = filter (not . isBouncy) l
    nonBouncyCount = toInteger (length nonBouncy)
    fullBouncy = a ++ nonBouncy
    fullBouncyCount = nonBouncyCount + b

xnlvl 0 = fstlvl
xnlvl n = nlvl (xnlvl (n-1))

pp (_,b,_) = b

main :: IO ()
main = do
  -- print $ fstlvl
  -- print $ sndlvl
  --print $ pp (xnlvl 0)
  --print $ pp (xnlvl 1)
  --print $ pp (xnlvl 2)
  --print $ pp (xnlvl 3)
  --print $ pp (xnlvl 4)
  --print $ pp (xnlvl 5)
  --print $ pp (xnlvl 6)
  --print $ pp (xnlvl 7)
  -- print $ pp (xnlvl 9)
  -- print $ xnlvl 3

  --print $ numBouncy 1 538
  -- print $ nextF 0.99 (stats2Fold 0.8 0 1)
  -- print $ bar (stats2Fold 0.5 0 1)
  bar2 (stats2Fold 0.5 0 1) >>= print 
  -- print $ stats2Fold 0.5 0 1
  -- print $ stats2Fold 0.6 0 1
  -- print $ stats2Fold 0.7 0 1
  --print $ stats2Fold 0.8 0 1
  --print $ stats2Fold 0.9 0 1
