-- TASK 1: Weather Report (Pattern Matching)
weatherReport :: String -> String
weatherReport "sunny" = "It's a bright and beautiful day!"
weatherReport "rainy" = "Don't forget your umbrella!"
weatherReport "cloudy" = "A bit gloomy, but no rain yet!"

weatherReport _       =  "Weather unknown"

-- TASK 2: Day Type Checker
dayType :: String -> String
dayType "Saturday" = "It's a weekend!"
dayType "Sunday" = "It's a weekend!"

dayType "Monday" = "It's a weekday."
dayType "Tuesday" = "It's a weekday."
dayType "Wednesday" = "It's a weekday."
dayType "Thursday" = "It's a weekday."
dayType "Friday" = "It's a weekday."

dayType _ = "Invalid day"

-- TASK 3: Grade Comment
gradeComment :: Int -> String
gradeComment score
  | score >= 90 && score <= 100 = "Excellent!"
  | score >= 70 && score <= 89 = "Good job!"
  | score >= 50 && score <= 69 = "You passed."
  | score >= 0 && score <= 49 = "Better luck next time."
  | otherwise = "Invalid grade"

-- TASK 4: Special Birthday (Patterm Matching)
specialBirthday :: Int -> String
specialBirthday 1 = "First birthday!"
specialBirthday 18 = "You're and adult!"
specialBirthday 21 = "Now you can drink!"
specialBirthday 50 = "Half a century!"
specialBirthday 65 = "First Retirement!"

specialBirthday age = "Happy " ++ show age ++ "th birthday!"

-- TASK 6: What's Inside This List?
whatsInsideThisList :: [a] -> String
whatsInsideThisList [] = "It is empty!"
whatsInsideThisList [x] = "A single element."
whatsInsideThisList [x, y] = "Two elements."
whatsInsideThisList (x: xs) = "The list is long!"

-- TASK 7: Ignore Elemenets (First and Third)
firstAndThird :: [Int] -> String
firstAndThird (x:_:z:_) = "First: " ++ show x ++ ", Third: " ++ show z
firstAndThird _ = "List is too short!"

-- TASK 8: Describe Tuple (Unpacking)
describeTuple :: (String, Int) -> String
describeTuple (name, age) =
  "Name: " ++ name ++ ", Age: " ++ show age

main :: IO ()
main = do
  putStrLn "--- Weather Report Test ---"
  print (weatherReport "sunny")
  print (weatherReport "rainy")
  print (weatherReport "snowing")
  
  putStrLn "--- Day Type Test ---"
  print (dayType "Sunday")
  print (dayType "Tuesday")
  print (dayType "Funday")
  
  putStrLn "--- Grade Comment Test ---"
  print (gradeComment 95)
  print (gradeComment 40)
  print (gradeComment 105)
  print (gradeComment (-5))
  
  putStrLn "--- Special Birthday Test ---"
  print (specialBirthday 18)
  print (specialBirthday 50)
  print (specialBirthday 33)
  
  putStrLn "--- Custom Message Test ---"
  print (specialBirthday 35)
  print (specialBirthday 12)
  
  putStrLn "--- List Content Test ---"
  print (whatsInsideThisList [])
  print (whatsInsideThisList [1])
  print (whatsInsideThisList [1, 2])
  print (whatsInsideThisList [1, 2, 3, 4, 5])
  
  putStrLn "--- First and Third Test ---"
  print (firstAndThird [1, 2, 3, 4, 5])
  print (firstAndThird [10,20, 30])
  print (firstAndThird [1, 2])
  
  putStrLn "--- Tuple Test ---"
  print (describeTuple ("ALice", 25))
  print (describeTuple ("Bob", 30))
