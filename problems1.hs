fizzbuzz :: [Int] -> [String]
fizzbuzz [] = []
fizzbuzz (a : b)
  | a `mod` 15 == 0 = "FizzBuzz" : fizzbuzz b
  | a `mod` 5 == 0 = "Fizz" : fizzbuzz b
  | a `mod` 3 == 0 = "Buzz" : fizzbuzz b
  | otherwise = show a : fizzbuzz b

filterVowels :: String -> String
filterVowels str = [ch | ch <- str, ch `elem` vowels]
  where
    vowels = ['a', 'e', 'i', 'o', 'A', 'E', 'I', 'O']

pythagoreanTriples :: Int -> [(Int, Int, Int)]
pythagoreanTriples n = [(a, b, c) | c <- [1 .. n], b <- [1 .. c], a <- [1 .. b], c ^ 2 == a ^ 2 + b ^ 2]

countElems :: (Eq a) => a -> [a] -> Int
countElems ch [] = 0
countElems ch (first : rest)
  | ch == first = 1 + countElems ch rest
  | otherwise = 0 + countElems ch rest

main :: IO ()
main = do
  print (fizzbuzz [1 .. 10])
  print (filterVowels "Hello, world!")
  print (pythagoreanTriples 20)
  print (countElems 'a' "Haskell is awesome!")
