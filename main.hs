main :: IO ()
main = print $ e1 10

e1 under = sum (map mulGen muls) - clashes
  where 
    mulGen mul = sum [mul,2*mul..under-1]
    muls = [3,5]
    clashes = mulGen (3*5)
