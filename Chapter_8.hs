-- Task 1: Type Synonyms and Basic Function
type Address = String
type Value   = Int

generateTx :: Address -> Address -> Value -> String
generateTx sender receiver amount = 
    "Transaction: [" ++ sender ++ "] sent [" ++ receiver ++ "] an amount of " ++ show amount

-- Task 2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = Person String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = Person "Bob" ("Maple Street", 42) Cash

-- Task 3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h

-- Task 4: Record Syntax for Employee
data Employee = Employee { name :: String
                         , experienceInYears :: Float
                         } deriving (Show)

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

-- Task 5: Record Syntax for Person
data Person2 = Person2 { name2      :: String  
                       , age        :: Int
                       , isEmployed :: Bool
                       } deriving (Show)

person1 :: Person2
person1 = Person2 { name2 = "Alice"
                 , age = 28
                 , isEmployed = True 
                 }

person2 :: Person2
person2 = Person2 { isEmployed = False
                 , name2 = "Bob"
                 , age = 35 
                 }

-- Task 6: Record Syntax for Shape Variants
data RecordShape = Oval { center :: (Float, Float)
                          , radius :: Float
                          , color  :: String 
                          }
                 | Square { width  :: Float
                             , height :: Float
                             , color  :: String 
                             } deriving (Show)

myOval :: RecordShape
myOval = Oval { center = (0.0, 0.0)
                  , radius = 5.0
                  , color  = "Red" 
                  }

mySquare :: RecordShape
mySquare = Square { width  = 10.0
                   , height = 5.0
                   , color  = "Blue" 
                   }

-- Task 7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving (Show)

describeAnimal :: Animal -> String
describeAnimal (Dog n) = "Woof! I am a dog named " ++ n ++ "."
describeAnimal (Cat n) = "Meow! I am a lovely cat named " ++ n ++ "."

myDog :: Animal
myDog = Dog "Buddy"

myCat :: Animal
myCat = Cat "Whiskers"

-- Task 8: Type Synonyms and Greeting Function
type Name = String
type Age  = Int

greet :: Name -> Age -> String
greet n a = "Hello there, " ++ n ++ "! I see you are " ++ show a ++ " years old."

-- Task 9: Record Type and Transaction Function
-- (Address and Value are already defined at the top of the file!)

data Transaction = Transaction { from          :: Address
                               , to            :: Address
                               , amount        :: Value
                               , transactionId :: String
                               } deriving (Show)

createTransaction :: Address -> Address -> Value -> String
createTransaction sender receiver val = 
    -- Generate a mock ID
    let generatedId = "TX-" ++ sender ++ "-" ++ receiver ++ "-" ++ show val
        
        -- Create the Transaction record in memory
        newTx = Transaction { from          = sender
                            , to            = receiver
                            , amount        = val
                            , transactionId = generatedId
                            }
                            
    -- Return ONLY the transactionId using the automatic getter
    in transactionId newTx

-- Task 10: Deriving Show for Book

-- 1. Define the Book type using Record Syntax and derive the Show club
data Book = Book { title  :: String
                 , author :: String
                 , year   :: Int
                 } deriving (Show)

-- 2. Create our specific book instance
myBook :: Book
myBook = Book { title  = "The Hitchhiker's Guide to the Galaxy"
              , author = "Douglas Adams"
              , year   = 1979
              }

main :: IO ()
main = do
    putStrLn "--- Type Synonym Test ---"
    let aliceWallet = "AliceWallet_0x123"
    let bobWallet   = "BobWallet_0xABC"
    let money = 500
    
    let receipt = generateTx aliceWallet bobWallet money
    putStrLn receipt
    
    putStrLn "\n--- Data Constructor Test ---"
    print bob 
    
    putStrLn "\n--- Shape Area Test ---"
    let myCircle = Circle 5.0
    let myRect   = Rectangle 10.0 5.0

    putStrLn "Area of the Circle (Radius 5):"
    print (area myCircle)  
    
    putStrLn "\nArea of the Rectangle (10x5):"
    print (area myRect) 
    
    putStrLn "\n--- Record Syntax Test ---"
    putStrLn "Full Record:"
    print richard
    
    putStrLn "\n--- Using the Automatic Getters ---"
    putStrLn ("Employee Name: " ++ name richard)
    putStrLn ("Years of Experience: " ++ show (experienceInYears richard))
    
    putStrLn "\n--- Person Record Test ---"
    putStrLn "Person 1 (Employed):"
    print person1
    
    putStrLn "\nPerson 2 (Unemployed):"
    print person2

    putStrLn ("\nIs " ++ name2 person1 ++ " employed? " ++ show (isEmployed person1))
    putStrLn ("Is " ++ name2 person2 ++ " employed? " ++ show (isEmployed person2))
    
    putStrLn "\n--- Record Syntax with Variants Test ---"
    putStrLn "Our Oval:"
    print myOval
    
    putStrLn "\nOur Square:"
    print mySquare
    
    putStrLn "\n--- Testing the Shared Getter ---"
    putStrLn ("The Oval is " ++ color myOval)
    putStrLn ("The Square is " ++ color mySquare)
    
    putStrLn "\n--- Animal Description Test ---"
    putStrLn "In Memory:"
    print myDog
    print myCat

    putStrLn "\nDescriptions:"
    putStrLn (describeAnimal myDog)
    putStrLn (describeAnimal myCat)
    
    putStrLn "\n--- Greeting Synonym Test ---"
    let myName = "Alice"
    let myAge  = 28
    
    let message = greet myName myAge
    putStrLn message
    
    putStrLn "\n--- Record Transaction Test ---"
    let aliceWallet2 = "Alice_0x1"
    let bobWallet2   = "Bob_0x2"
    let transferAmt  = 250
    
    -- Run the function and capture the resulting ID
    let resultingId = createTransaction aliceWallet2 bobWallet2 transferAmt
    
    putStrLn ("Processed transfer! The receipt ID is: " ++ resultingId)
    
    putStrLn "--- Book Record Show Test ---"

    print myBook
