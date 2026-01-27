import Data.Char (isUpper)

-- Task 1: Apply Three Times (Higher-Order Function)
applyThreeTimes :: (Int -> Int) -> Int -> Int
applyThreeTimes f x = f (f (f x))

-- Task 2: Filter Odd Numbers
getOdds :: [Int] -> [Int]
getOdds list = filter odd list

-- Task 3: Check for Uppercase Words
startsUpper :: String -> Bool
startsUpper word = isUpper (head word)

hasUppercase :: [String] -> Bool
hasUppercase list = any startsUpper list

-- Task 4: Lambda Rewrite
biggerThan10 :: Int -> Bool
biggerThan10 = \x -> x > 10

-- Task 5: Partial Application
multiplyByFive :: Int -> Int
multiplyByFive = (* 5)

-- Task 6: Function Composition
square :: Int -> Int
square x = x * x

processList :: [Int] -> [Int]
processList = filter even . map square

-- TASK 7: The $ Operator (Function Application)
result :: Int
result = sum $ map (*2) $ filter (>3) [1..10]

-- TASK 8: Point-Free Style
addFive :: Int -> Int
addFive = (+ 5)

-- TASK 9: Transform List (Apply Twice to All)
transformList :: (a -> a) -> [a] -> [a]
transformList f list = map (\x -> f (f x)) list

-- TASK 10: Combining Higher-Order Functions
checkSquared :: [Int] -> Bool
checkSquared = any (>50) . map (^2)

main :: IO ()
main = do
    putStrLn "--- Apply Three Times Test ---"
    print (applyThreeTimes (+2) 10)
    print (applyThreeTimes (*10) 1)
    print (applyThreeTimes (\x -> x * x) 2)
    
    putStrLn "--- Odd Numbers Test ---"
    let myList = [1..30]
    print (getOdds myList)
    
    putStrLn "--- Any Uppercase Test ---"
    let list1 = ["apple", "code", "Haskell", "fun"]
    print (hasUppercase list1)
    let list2 = ["apple", "code", "fun"]
    print (hasUppercase list2)
    
    putStrLn "--- Lambda Test ---"
    print (biggerThan10 5)   
    print (biggerThan10 15)  
    print (filter (\x -> x > 10) [5, 8, 12, 15])
    
    putStrLn "--- Partial Application Test ---"
    print (multiplyByFive 10)  
    print (multiplyByFive 4)   
    print (map multiplyByFive [1, 2, 3]) 
    
    putStrLn "--- Composition Test ---"
    let numbers = [1, 2, 3, 4, 5]
    print (processList numbers)
    
    putStrLn "--- Dollar Operator Test ---"
    print result
    
    putStrLn "--- Point-Free Test ---"
    print (addFive 10)
    print (addFive 2)
    
    putStrLn "--- Transform List Test ---"
    let numbers = [1, 2, 3, 4, 5]
    print (transformList (+1) numbers)
    print (transformList (^2) [2, 3])
    
    putStrLn "--- Combining Higher-Order Functions Test ---"
    print (checkSquared [1, 2, 3])
    print (checkSquared [1, 5, 8])
