divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..div x 2], rem x y == 0]

isAbundant :: Integer -> Bool
isAbundant x = (sum $ divisors x) > x

getAbundantNums :: Integer -> [Integer]
getAbundantNums x = filter isAbundant [1..x]
