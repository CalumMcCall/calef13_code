--Solution to project euler problem 14

--This function is taken from the first answer here 
--http://stackoverflow.com/questions/3646330/how-to-improve-the-performance-of-this-haskell-program
nextNum ::Integer -> Integer
nextNum x
        | r == 0    = q
        | otherwise = 6*q+4
        where (q,r) = quotRem x 2

seqLen :: Integer -> Integer
seqLen x
    | result == 1   = 1
    | otherwise     = 1 + seqLen result
    where result = nextNum x

main = do putStrLn $ show $ snd $ maximum lengths
          where lengths = [(seqLen a, a) | a <- [1..1000000]]
