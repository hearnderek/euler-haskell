fib = 1 : 1 : zipWith (+) fib (tail fib)

main = putStrLn (show (sum (filter even (takeWhile (<4000000) fib))))
