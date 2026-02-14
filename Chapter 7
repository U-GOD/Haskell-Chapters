import Text.Read (readMaybe)

-- Combined Tasks 1 & 2: Custom Data Type, Eq, and Ord Instances
data Color = Red | Green | Blue deriving Show

instance Eq Color where
    Red == Red     = True
    Green == Green = True
    Blue == Blue   = True
    _ == _         = False

instance Ord Color where
    compare Red Red     = EQ
    compare Green Green = EQ
    compare Blue Blue   = EQ
    compare Red _       = LT
    compare Blue _      = GT
    compare Green Red   = GT
    compare Green Blue  = LT

-- Task 3: Function Using Multiple Constraints
compareValues :: (Eq a, Ord a) => a -> a -> a
compareValues x y
    | x > y     = x
    | otherwise = y

-- Task 4: Custom Type with Show and Read
data Shape = Circle Double | Rectangle Double Double 
    deriving (Show, Read)

-- Task 5: Function with Num Constraint
squareArea :: Num a => a -> a
squareArea side = side * side
    
-- Task 6: Using Integral and Floating Type Classes
circleCircumference :: (Real a, Floating b) => a -> b
circleCircumference r = 2 * pi * realToFrac r    

-- Task 7: Bounded and Enum
data Color2 = Yellow | Black | Purple deriving (Show, Eq, Enum, Bounded)

nextColor :: Color2 -> Color2
nextColor c
    | c == maxBound = minBound 
    | otherwise     = succ c  

-- Task 8: Parse a Value from a String Using Read
data Shape2 = Oval Double | Cube Double Double 
    deriving (Show, Read)

parseShape :: String -> Maybe Shape2
parseShape input = readMaybe input

-- Task 9 & 10 Combined: Type Class with Multiple Instances
class Describable a where
    describe :: a -> String

instance Describable Bool where
    describe True  = "A boolean value representing absolute truth."
    describe False = "A boolean value representing falsehood."

instance Describable Shape where
    describe (Circle r)      = "A perfectly round circle with a radius of " ++ show r ++ "."
    describe (Rectangle w h) = "A rectangle with a width of " ++ show w ++ " and a height of " ++ show h ++ "."

instance Describable Int where
    describe n = "The mighty number " ++ show n

describeAndCompare :: (Describable a, Ord a) => a -> a -> String
describeAndCompare x y
    | x > y     = describe x 
    | otherwise = describe y 

-- Entry Point
main :: IO ()
main = do
    putStrLn "--- Custom Color Eq Test (Task 1) ---"
    let color1 = Red
    let color2 = Red
    let color3 = Blue
    print (color1 == color2)  
    print (color1 == color3)  
    print (Green /= Blue)     
    
    putStrLn "\n--- Custom Color Ord Test (Task 2) ---"
    print (Red < Green)        
    print (Blue > Green)      
    print (Red > Blue)        
    print (compare Green Blue)
    print (maximum [Red, Blue, Green])
    
    putStrLn "\n--- Multiple Constraints Test (Task 3) ---"
    print (compareValues 10 20)          
    print (compareValues 99 5)            
    print (compareValues "Apple" "Zebra") 
    print (compareValues False True)
    
    putStrLn "\n--- Show Test (Memory to String) (Task 4) ---"
    let myCircle = Circle 5.5
    let myRect = Rectangle 10.0 4.2
    print myCircle
    putStrLn ("I made a shape: " ++ show myRect)
    putStrLn "\n--- Read Test (String to Memory) ---"
    let rawText = "Circle 9.9"
    let parsedShape = read rawText :: Shape
    print parsedShape
    
    putStrLn "\n--- Polymorphic Math Test (Task 5) ---"
    let intSide = 5 :: Int
    putStrLn "Area with Int:"
    print (squareArea intSide)      
    let doubleSide = 5.5 :: Double
    putStrLn "\nArea with Double:"
    print (squareArea doubleSide)   
    putStrLn "\nArea with Automatic Type Inference:"
    print (squareArea 10) 
    
    putStrLn "\n--- Mixed Math Test (Task 6) ---"
    let intRadius = 10 :: Int
    putStrLn "Circumference with Int radius (10):"
    print (circleCircumference intRadius)
    let doubleRadius = 5.5 :: Double
    putStrLn "\nCircumference with Double radius (5.5):"
    print (circleCircumference doubleRadius)
    
    putStrLn "\n--- Enum and Bounded Test (Task 7) ---"
    let c1 = Yellow
    let c2 = Black
    let c3 = Purple
    putStrLn ("Next after Yellow: " ++ show (nextColor c1))   
    putStrLn ("Next after Black: " ++ show (nextColor c2)) 
    putStrLn ("Next after Purple: " ++ show (nextColor c3))  
    
    putStrLn "\n--- Safe Parsing Test (Task 8) ---"
    let validCircle = "Oval 5.5"
    putStrLn ("Parsing '" ++ validCircle ++ "':")
    print (parseShape validCircle)  
    
    let validRect = "Cube 10.0 4.2"
    putStrLn ("\nParsing '" ++ validRect ++ "':")
    print (parseShape validRect)
    
    let badInput1 = "Banana"
    putStrLn ("\nParsing '" ++ badInput1 ++ "':")
    print (parseShape badInput1)
    
    let badInput2 = "Oval \"Five\""
    putStrLn ("\nParsing '" ++ badInput2 ++ "':")
    print (parseShape badInput2)  
    
    putStrLn "\n--- Custom Type Class Test (Task 9) ---"
    putStrLn (describe True)
    putStrLn (describe False)
    
    putStrLn "" 
    putStrLn (describe myCircle)
    putStrLn (describe myRect)
    
    putStrLn "\n--- Double Constraint Test (Task 10) ---"
    putStrLn "Comparing 10 and 99:"
    putStrLn (describeAndCompare (10 :: Int) (99 :: Int))

    putStrLn "\nComparing False and True:"
    putStrLn (describeAndCompare False True)
