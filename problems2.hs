-- 1. Sum of Squares

-- Write a function that takes a list of integers and
-- returns the sum of the squares of all even numbers
-- in the list.

sumOfSquares :: [Int] -> Int
sumOfSquares xs = sum [x * x | x <- xs, even x]

-- 2. Reverse a String

-- Create a function that takes a string and returns
-- the reverse of that string

reverseString :: String -> String
reverseString str = reverseString str []
  where
    reverseString [] a = a
    reverseString (x : xs) a = reverseString xs (x : a)

-- 3. Prime Checker

-- Write a function that checks if a given number is prime.
-- A prime number is only divisible by 1 and itself.

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = null [x | x <- [2 .. floor (sqrt (fromIntegral n))], n `mod` x == 0]

-- 4. Find the Maximum

-- Create a function that finds the maximum element in a
-- list without using the built-in maximum function.

findMax :: (Ord a) => [a] -> a
findMax [x] = x
findMax (x : xs) = findMax x xs
  where
    findMax currentMax [] = currentMax
    findMax currentMax (x : xs)
      | x > currentMax = findMax x xs
      | otherwise = findMax currentMax xs

-- 5. Count Vowels and Consonants

-- Create a function that counts the number of vowels
-- and consonants in a given string and returns a tuple with both counts.
countVowelsAndConsonants :: String -> (Int, Int)
countVowelsAndConsonants "" = (0, 0)
countVowelsAndConsonants (x : xs)
  | x `elem` vowels = countVowelsAndConsonants xs (1, 0)
  | x `elem` consonants = countVowelsAndConsonants xs (0, 1)
  | otherwise = countVowelsAndConsonants xs (0, 0)
  where
    vowels = ['a', 'e', 'i', 'o', 'A', 'E', 'I', 'O']
    consonants = [x | x <- ['a' .. 'z'], x `notElem` vowels] ++ [x | x <- ['A' .. 'Z'], x `notElem` vowels]
    countVowelsAndConsonants "" (v, c) = (v, c)
    countVowelsAndConsonants (x : xs) (v, c)
      | x `elem` vowels = countVowelsAndConsonants xs (v + 1, c)
      | x `elem` consonants = countVowelsAndConsonants xs (v, c + 1)
      | otherwise = countVowelsAndConsonants xs (v, c)

main :: IO ()
main = do
  let list = [1 .. 5]

  print (sumOfSquares list)
  print (reverseString "Hello, world!")
  print (isPrime 5)
  print (findMax [3, 5, 9, 7, 2])
  print (countVowelsAndConsonants "")
