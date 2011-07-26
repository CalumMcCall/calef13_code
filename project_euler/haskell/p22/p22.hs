import Char
import IO
import List
strip :: String -> String
strip []     = []
strip (x:xs)
        | isAlpha x = x:strip xs
        | otherwise = ' ':strip xs

scoreWord :: String -> Integer
scoreWord []     = 0
scoreWord (x:xs) = score x + scoreWord xs
             where score x = toInteger $ (fromEnum x - fromEnum 'A') + 1

scoreWords :: [String] -> Integer -> Integer
scoreWords [] _     = 0
scoreWords (x:xs) n = scoreWord x * n + scoreWords xs (n+1)

main = do
       handle <- openFile "names.txt" ReadMode
       contents <- hGetContents handle
       let sortedNames = sort $ words $ strip contents
       putStrLn $ show $ scoreWords sortedNames 1
       hClose handle

