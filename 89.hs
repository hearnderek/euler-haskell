rns :: [Integer]
rns = [1,5,10,50,100,500,1000,5000,10000]

rn :: Integer -> [Integer]
rn n 
  | n >= 10000 = 10000:(rn (n-10000))
  | n >= 5000 = 5000:(rn (n-5000))
  | n >= 1000 = 1000:(rn (n-1000))
  | n >= 500 = 500:(rn (n-500))
  | n >= 100 = 100:(rn (n-100))
  | n >= 50 = 50:(rn (n-50))
  | n >= 10 = 10:(rn (n-10))
  | n >= 5 = 5:(rn (n-5))
  | n == 4 = 1:5:(rn (n-4))
  | n >= 1 = 1:(rn (n-1))
  | otherwise = [] 
  where 
    f n x = x:(rn(n-x)) 

main :: IO ()
main = print $ rn 4
