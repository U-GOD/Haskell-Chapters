import Text.Printf

-- TASK 1 Check Number
checkNumber :: Int -> String
checkNumber x = 
  if x > 0
    then "Positive"
    else if x < 0
      then "Negative"
      else "Zero"
      
-- TASK 2 Grades (Using Guards)
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

-- TASK 3 RGB to Hex
rgbToHex :: (Int, Int, Int) -> String
rgbToHex (r, g, b) = 
  let rHex = printf "%02x" r
      gHex = printf "%02x" g
      bHex = printf "%02x" b
  
  in rHex ++ gHex ++ bHex

-- TASK 4: Heron's Formula
triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = 
  let s = (a + b + c) / 2
  
  in sqrt (s * (s - a) * (s - b) * (s - c))

-- TASK 5: Triangle Type
triangleType :: Float -> Float -> Float -> String
triangleType a b c
  | a == b && b == c = "Equilateral"
  | a == b || b == c || a == c = "Isosceles"
  | otherwise = "Scalene"

-- TASK 6: Leap Year Check
isLeapYear :: Int -> Bool
isLeapYear year = 
  if year `mod` 400 == 0
     then True
     else if year `mod` 100 == 0
      then False
      else if year `mod` 4 == 0
        then True
        else False
        
-- TASK 7: Determine Season
season :: Int -> String
season m
  | m `elem` [12, 1, 2] = "Winter"
  | m `elem` [3, 4, 5] = "Spring"
  | m `elem` [6, 7, 8] = "Summer"
  | m `elem` [9, 10, 11] = "Autumn"
  | otherwise = "Invalid Month"

-- TASK 8: BMI Calculator using 'where'
bmiCategory :: Float -> Float -> String
bmiCategory weight height
  | bmi < 18.5 = "Underweight"
  | bmi < 25.0 = "Normal"
  | bmi < 30.0 = "Overweight"
  | otherwise = "Obese"
  
  where bmi = weight / (height ^ 2)

-- TASK 9: Max of Three (Using let)
maxOfThree :: Int -> Int -> Int -> Int
maxOfThree a b c = 
  let maxAB = max a b
  
  in max maxAB c

-- TASK 10: Palindrom Checker (Recursion)
isPalindrome :: String -> Bool
isPalindrome s
  | length s <= 1 = True
  | head s == last s = isPalindrome (init (tail s))
  | otherwise = False

main :: IO ()
main = do
  putStrLn "___ Task 1(Check Number) ___"
  print (checkNumber 5)
  print (checkNumber (-3))
  print (checkNumber 0)
  
  putStrLn "___ Task 2(Grading System ___"
  print (grade 95)
  print (grade 72)
  print (grade 50)
  
  putStrLn "--- Task 3(RGB to Hex Test) ---"
  print (rgbToHex (255, 0, 127))
  print (rgbToHex (0, 255, 64))
  
  putStrLn "--- Task 4(Triangle Area Test) ---"
  print (triangleArea 3 4 5)
  print (triangleArea 7 8 9)
  
  putStrLn "--- Task 5(Triangle Type Test) ---"
  print (triangleType 3 3 3)
  print (triangleType  5 5 8)
  print (triangleType 6 7 8)
  
  putStrLn "--- Task 6(Leap Year Test) ---"
  print (isLeapYear 2000)
  print (isLeapYear 1900)
  print (isLeapYear 2024)
  print (isLeapYear 2026)
  
  putStrLn "--- Task 7(Season test) ---"
  print (season 3)
  print (season 7)
  print (season 11)
  print (season 1)
  
  putStrLn "--- Task 8(BMI Test) ---"
  print (bmiCategory 70 1.75)
  print (bmiCategory 90 1.8)
  
  putStrLn "--- Task 9(Max of Three Test) ---"
  print (maxOfThree 10 20 30)
  print (maxOfThree 5 25 10)
  
  putStrLn "--- task 10(Palindrome Test) ---"
  
  putStrLn "Is 'racecar' a palindrome?"
  print (isPalindrome "racecar")
  
  putStrLn "Is 'haskell' a palindrome?"
  print (isPalindrome "haskell")
  
  putStrLn "Is 'madam' a palindrome?"
  print (isPalindrome "madam")
