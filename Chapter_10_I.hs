{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- Task 1: ShowSimple Type Class
data PaymentMethod = Cash | Card | Cryptocurrency 

-- 2. Define the Type Class (The Contract)
class ShowSimple a where
    showSimple :: a -> String

-- 3. Define the Instance (The Membership)
instance ShowSimple PaymentMethod where
    showSimple Cash           = "Paid in Cash"
    showSimple Card           = "Paid with Credit/Debit Card"
    showSimple Cryptocurrency = "Paid via Crypto Wallet"

-- Task 2: Summable Type Class
class Summable a where
    sumUp :: [a] -> a

instance Summable Int where
    sumUp [] = 0
    sumUp (x:xs) = x + sumUp xs

-- Task 3: Comparable Type Class
-- A simple blockchain modeled by its length
data Blockchain = Blockchain { chainLength :: Int } deriving (Show)

class Comparable a where
    compareWith :: a -> a -> Ordering

instance Comparable Blockchain where
    compareWith bc1 bc2
        | chainLength bc1 < chainLength bc2 = LT
        | chainLength bc1 > chainLength bc2 = GT
        | otherwise                         = EQ

-- Task 4: Eq Instance for Box
data Box a = Empty | Has a deriving (Show)

instance Eq a => Eq (Box a) where
    Empty == Empty = True

    (Has x) == (Has y) = x == y

    _ == _ = False

-- Task 5: ShowDetailed Type Class
data User = User { name :: String
                 , age  :: Int
                 , role :: String 
                 } deriving (Eq, Show) 

class ShowDetailed a where
    showDetailed :: a -> String

instance ShowDetailed User where
    showDetailed u = 
        "=== USER PROFILE ===\n" ++
        "Name: " ++ name u ++ "\n" ++
        "Age:  " ++ show (age u) ++ " years old\n" ++
        "Role: " ++ role u ++ "\n" ++
        "===================="

-- Task 6: Mutual Recursion in Eq for Blockchain
-- Define Custom Type Class with Mutual Recursion
class MyEq a where
    (===) :: a -> a -> Bool
    (=/=) :: a -> a -> Bool

    x === y = not (x =/= y)
    x =/= y = not (x === y)

-- Define  Instance
instance MyEq Blockchain where
    bc1 === bc2 = chainLength bc1 == chainLength bc2

-- Task 7: Convertible Type Class

-- Define  Multi-Parameter Type Class
-- Read as: "If type 'a' can be converted into type 'b', you must provide a convert function."
class Convertible a b where
    convert :: a -> b

-- Define Instance (The Membership)
-- Here, 'a' becomes PaymentMethod, and 'b' becomes String!
instance Convertible PaymentMethod String where
    convert Cash           = "Converted: Cash Payment"
    convert Card           = "Converted: Card Payment"
    convert Cryptocurrency = "Converted: Crypto Payment"

-- Task 8: AdvancedEq Subclass of Eq
class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq Int where
    compareEquality x y = x == y

-- Task 9: MinMax Type Class
class MinMax a where
    minValue :: a
    maxValue :: a
    
instance MinMax Int where
    minValue = minBound
    maxValue = maxBound

-- Task 10: Concatenatable Type Class
class Concatenatable a where
    concatWith :: a -> a -> a

instance Concatenatable [Char] where
    concatWith str1 str2 = str1 ++ str2

main :: IO ()
main = do
    putStrLn "--- Custom Type Class Test ---"
    let myPayment = Card
    let yourPayment = Cryptocurrency
    putStrLn (showSimple myPayment)
    putStrLn (showSimple yourPayment)
    
    putStrLn "\n--- Summable Type Class Test ---"
    let myNumbers = [10, 20, 30, 42] :: [Int]
    putStrLn ("Our list of numbers: " ++ show myNumbers)
    let total = sumUp myNumbers
    putStrLn ("The total sumUp value is: " ++ show total)
    
    putStrLn "\n--- Comparable Type Class Test ---"
    let mainNet = Blockchain { chainLength = 5000 }
    let testNet = Blockchain { chainLength = 150 }
    let forkNet = Blockchain { chainLength = 5000 }
    putStrLn "Comparing mainNet to testNet:"
    print (compareWith mainNet testNet) 
    putStrLn "Comparing testNet to mainNet:"
    print (compareWith testNet mainNet) 
    putStrLn "Comparing mainNet to forkNet:"
    print (compareWith mainNet forkNet) 
    
    putStrLn "\n--- Eq Instance for Box Test ---"
    let box1 = Has 42
    let box2 = Has 42
    let box3 = Has 99
    let emptyBox1 = Empty :: Box Int
    let emptyBox2 = Empty :: Box Int
    putStrLn "Does box1 equal box2? (42 vs 42)"
    print (box1 == box2)  
    putStrLn "Does box1 equal box3? (42 vs 99)"
    print (box1 == box3)  
    putStrLn "Does emptyBox1 equal emptyBox2?"
    print (emptyBox1 == emptyBox2)
    putStrLn "Does box1 NOT equal box3? (Testing automatic /=)"
    print (box1 /= box3)
    
    putStrLn "\n--- ShowDetailed Type Class Test ---"
    let adminUser = User { name = "Grace Hopper", age = 85, role = "System Administrator" }
    putStrLn "Developer View (standard Show):"
    print adminUser
    putStrLn "\nEnd-User View (custom ShowDetailed):"
    putStrLn (showDetailed adminUser)
    
    putStrLn "\n--- Mutual Recursion MyEq Test ---"
    
    let mainNet = Blockchain { chainLength = 5000 }
    let testNet = Blockchain { chainLength = 150 }
    let forkNet = Blockchain { chainLength = 5000 }
    
    putStrLn "Are mainNet and forkNet equal? (Testing ===)"
    print (mainNet === forkNet)  
    
    putStrLn "Are mainNet and testNet NOT equal? (Testing the free =/=)"
    print (mainNet =/= testNet) 
    
    putStrLn "\n--- MultiParam Convertible Test ---"
    let myCash = Cash
    putStrLn (convert myCash)
    
    putStrLn "\n--- AdvancedEq Subclass Test ---"
    
    let num1 = 5 :: Int
    let num2 = 5 :: Int
    let num3 = 10 :: Int
    
    putStrLn "Does 5 advanced-equal 5?"
    print (compareEquality num1 num2)  
    
    putStrLn "Does 5 advanced-equal 10?"
    print (compareEquality num1 num3) 
    
    putStrLn "\n--- MinMax Type Class Test ---"
    putStrLn ("The minimum Int value is: " ++ show (minValue :: Int))
    putStrLn ("The maximum Int value is: " ++ show (maxValue :: Int))
    
    putStrLn "\n--- Concatenatable Type Class Test ---"
    
    let firstHalf = "Hello, "
    let secondHalf = "Haskell!"
    
    putStrLn ("Combining strings: " ++ concatWith firstHalf secondHalf)
