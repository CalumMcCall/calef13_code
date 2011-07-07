--All numbers after max can be expressed as sum of two abundant numbers
--so no point in checking higher
let max = 28123

divisors :: Integer -> [Integer]
divisors x = [y | y <- [1..div x 2], rem x y == 0]

isAbundant :: Integer -> Bool
isAbundant x = (sum $ divisors x) > x

getAbundantNums :: Integer -> [Integer]
getAbundantNums x = filter isAbundant [1..x]

main = do   let abNums = getAbundantNums max
            putStrLn "Generated required abundant numbers"
            noAbNums = [x | x <- [1..max], elem x abNums]

