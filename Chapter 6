-- Task 1: Recursive Factorial
factorial :: Int -> Int

factorial 0 = 1
factorial n = n * factorial (n - 1)

-- Task 2: Recursive Fibonacci
fibonacci :: Int -> Int

fibonacci 0 = 0
fibonacci 1 = 1

fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

-- Task 3: Sum with foldr
sumList :: [Int] -> Int
sumList = foldr (+) 0

-- Task 4: Product with foldl
productList :: [Int] -> Int
productList = foldl (*) 1

-- Task 5: Recursive Reverse
reverseList :: [a] -> [a]
reverseList [] = []

reverseList (x:xs) = reverseList xs ++ [x]

-- Task 6: Element Exists (Recursive Search)
exists :: (Eq a) => a -> [a] -> Bool

exists _ [] = False
exists n (x:xs)
    | n == x    = True          
    | otherwise = exists n xs 
    
-- Task 7: List Length (Recursive)
listLength :: [a] -> Int
listLength [] = 0

listLength (_:xs) = 1 + listLength xs
    
-- Task 8: Filter Evens (Recursive)
filterEvens :: [Int] -> [Int]
filterEvens [] = []

filterEvens (x:xs)
    | even x    = x : filterEvens xs  
    | otherwise = filterEvens xs    

-- Task 9: Map Implementation (Recursive)
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []

myMap f (x:xs) = f x : myMap f xs

-- Task 10: Digits of a Number (Recursive)
getDigits :: Int -> [Int]
getDigits n
    | n < 10    = [n]
    
getDigits n = getDigits (n `div` 10) ++ [n `mod` 10]

main :: IO ()
main = do
    putStrLn "--- Factorial Test ---"
    print (factorial 5)  
    print (factorial 0)  
    
    putStrLn "--- Fibonacci Test ---"
    print (fibonacci 6)
    print (fibonacci 10)
    
    putStrLn "--- Sum Test ---"
    print (sumList [1, 2, 3, 4, 5]) 
    print (sumList []) 
    
    putStrLn "--- Product Test ---"
    print (productList [2, 3, 4]) 
    print (productList []) 
    
    putStrLn "--- Reverse List Test ---"
    print (reverseList [1, 2, 3, 4, 5])
    print (reverseList "hello") 
    
    putStrLn "--- Exists Test ---"
    let numbers = [10, 20, 30, 40, 50]
    print (exists 30 numbers)
    print (exists 99 numbers)
    
    putStrLn "--- List Length Test ---"
    print (listLength [10, 20, 30])
    print (listLength "Hello")
    print (listLength []) 
    
    putStrLn "--- Filter Evens Test ---"
    let list = [1, 2, 3, 4, 5, 6]
    print (filterEvens list)
    
    putStrLn "--- Map Implementation Test ---"
    let numbers = [1, 2, 3, 4, 5]
    print (myMap (^2) numbers)
    print (myMap (++ "!") ["Hi", "There"])
    
    putStrLn "--- Digits Test ---"
    print (getDigits 12345)
    print (getDigits 7) 
