-- a permutation is an order arrangement of objects.
-- For example, 3124 is one possible permutation of the digits 1, 2, 3, 4.
-- If all of the permutations are listed numerically or alphabetically, we call it lexicographic order.
-- The lexicographic permutations of 0, 1 and 2 are: 012 021 102... etc

-- what is the millionth lexicographic permutiation of the digits [0..9] ?

nums = map show [0..9]

perms = 

main = do
  print nums
