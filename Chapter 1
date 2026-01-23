import Data.List (sortOn)
import Data.Ord (Down(..))


-- TASK 1
double :: Int -> Int
double x = x * 2

increment :: Int -> Int
increment x = x + 1

doubleThenIncrement :: Int -> Int
doubleThenIncrement = increment . double


-- TASK 2
circleArea :: Double -> Double
circleArea r = pi * r^2

-- TASK 3
greaterThan18 :: Int -> Bool
greaterThan18 g = g > 18


-- TASK 4
sortByScore :: [(String, Int)] -> [(String, Int)]
sortByScore list = sortOn (Down . snd) list

topThree :: [(String, Int)] -> [(String, Int)]
topThree list = take 3 list

extractPlayers :: [(String, Int)] -> [String]
extractPlayers list = map fst list

getTopThreePlayers :: [(String, Int)] -> [String]
getTopThreePlayers = extractPlayers . topThree . sortByScore

-- TASK 5
firstNNumbers :: Int -> [Int]
firstNNumbers n = take n [1..]

--TASK 6
addNumbers :: Int -> Int -> Int
addNumbers a b = a + b

-- TASK 7
fToC :: Double -> Double
fToC f = (f - 32) * (5 / 9)

-- TASK 8
increment2 :: Int -> Int
increment2 x = x + 1

applyTwice :: (Int -> Int) -> Int -> Int
applyTwice f x = f (f x)

main :: IO ()
main = do
  print (doubleThenIncrement 5)
  print (circleArea 5.0)
  let players = [("Alice", 80), ("Bob", 100), ("Charlie", 50), ("Dave", 95), ("Eve", 90)]
  let winners = getTopThreePlayers players
  print winners
  print (greaterThan18 20)
  print (greaterThan18 5)
  print (firstNNumbers 10)
  print (firstNNumbers 5)
  print (addNumbers 2 3)
  print (fToC 98.6)
  print (applyTwice increment2 10)
