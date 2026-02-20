-- Task 1: Define a Parametric Type Synonym
-- A concrete synonym for our address
type Address = String

--  The Parametric Type Synonym!
type Entity a = (a, Address)

-- Using the synonym with a String
personEntity :: Entity String
personEntity = ("Alice", "123 Maple Street")

-- Using the same synonym with an Int
businessEntity :: Entity Int
businessEntity = (9942, "456 Corporate Blvd")

-- Task 2 & 3: Parametric Data Type and Function
data Box a = Empty | Has a deriving (Show)

-- Task 2 Instances
intBox :: Box Int
intBox = Has 42

stringBox :: Box String
stringBox = Has "Hello, Haskell!"

emptyBox :: Box Double
emptyBox = Empty

-- Task 3: Function to Add Values in a Box
addN :: Num a => a -> Box a -> Box a
addN _ Empty   = Empty           
addN n (Has x) = Has (x + n)     

-- Task 4: Extract a Value from a Box
extract :: a -> Box a -> a

extract defaultVal Empty = defaultVal
extract _ (Has x) = x

-- Task 5: Parametric Data Type with Record Syntax
data Shape a = Circle { radius :: Float
                      , color  :: a 
                      }
             | Rectangle { width  :: Float
                         , height :: Float
                         , color  :: a 
                         } deriving (Show)

stringColorCircle :: Shape String
stringColorCircle = Circle { radius = 5.5
                           , color  = "Red" 
                           }

rgbColorRect :: Shape (Int, Int, Int)
rgbColorRect = Rectangle { width  = 10.0
                         , height = 4.0
                         , color  = (255, 128, 0) -- Orange in RGB
                         }

-- Task 6 & 7: Recursive Data Type for Tweets and Functions
data Tweet = Tweet { content  :: String
                   , likes    :: Int
                   , comments :: [Tweet]  
                   } deriving (Show)

-- Task 6 Instances
reply1 :: Tweet
reply1 = Tweet { content  = "I completely agree, recursion is mind-bending!"
               , likes    = 15
               , comments = [] 
               }

reply2 :: Tweet
reply2 = Tweet { content  = "Wait, what happens if a tweet replies to itself?"
               , likes    = 42
               , comments = [] 
               }

mainTweet :: Tweet
mainTweet = Tweet { content  = "Just learned about Recursive Data Types in Haskell!"
                  , likes    = 9000
                  , comments = [reply1, reply2] 
                  }

-- Task 7: Engagement Function for Tweets
engagement :: Tweet -> Int
engagement t = likes t + sum (map engagement (comments t))

replyA :: Tweet
replyA = Tweet "I love Haskell!" 10 []

replyB :: Tweet
replyB = Tweet "Recursion is fun." 5 []

middleTweet :: Tweet
middleTweet = Tweet "What is your favorite feature?" 20 [replyA, replyB]

topTweet :: Tweet
topTweet = Tweet "Learning Functional Programming today." 100 [middleTweet]

-- Task 8: Recursive Sequence Data Type
-- FIX: Changed 'Empty' to 'EmptySeq' to avoid clashing with the 'Box' empty!
data Sequence a = EmptySeq | Node a (Sequence a) deriving (Show)

-- Build a sequence of numbers
myNumSeq :: Sequence Int
myNumSeq = Node 1 (Node 2 (Node 3 EmptySeq))

-- Build a sequence of strings
myStringSeq :: Sequence String
myStringSeq = Node "Haskell" (Node "is" (Node "awesome" EmptySeq))

-- Task 9: Check for Element in a Sequence

-- (Assuming 'Sequence a' and 'EmptySeq' are defined above from Task 8
elemSeq :: Eq a => a -> Sequence a -> Bool

-- Rule 1: The Base Case
elemSeq _ EmptySeq = False

-- Rule 2: The Recursive Case
elemSeq target (Node x rest)
    | target == x = True                  -- We found it! Stop searching.
    | otherwise   = elemSeq target rest   -- Not here. Keep looking down the chain.

-- Task 10: Binary Search Tree Data Type

--  Define the Recursive Parametric Data Type
-- Read as: "A BST of 'a' is either an EmptyTree, 
-- or a TreeNode holding a value 'a', a left BST, and a right BST."
data BST a = EmptyTree | TreeNode a (BST a) (BST a) deriving (Show)

-- 2. Build a simple Binary Search Tree!
myTree :: BST Int
myTree = TreeNode 5 
            (TreeNode 3 EmptyTree EmptyTree)  
            (TreeNode 7 EmptyTree EmptyTree) 

main :: IO ()
main = do
    putStrLn "--- Parametric Type Synonym Test ---"
    
    putStrLn "Person Entity (String payload):"
    print personEntity
    
    putStrLn "\nBusiness Entity (Int payload):"
    print businessEntity
    
    putStrLn "\n--- Parametric Data Type Test ---"
    
    putStrLn "Box holding an Int:"
    print intBox
    
    putStrLn "\nBox holding a String:"
    print stringBox
    
    putStrLn "\nEmpty Box (Expecting a Double, but it's empty):"
    print emptyBox
    
    putStrLn "\n--- Box Math Test ---"
    let myBox = Has 10
    putStrLn "Original Box:"
    print myBox
    
    putStrLn "\nAdding 5 to the Box:"
    print (addN 5 myBox)        
    
    let myEmptyBox = Empty :: Box Int
    putStrLn "\nAdding 5 to an Empty Box:"
    print (addN 5 myEmptyBox)
    
    putStrLn "\n--- Safe Extraction Test ---"
    let myFullBox  = Has 99
    let myEmptyBox2 = Empty :: Box Int 

    putStrLn "Extracting from full box (fallback is 0):"
    print (extract 0 myFullBox)   

    putStrLn "\nExtracting from empty box (fallback is 0):"
    print (extract 0 myEmptyBox2)  

    let missingName = Empty :: Box String
    putStrLn "\nExtracting missing name:"
    putStrLn (extract "Unknown User" missingName) 
    
    putStrLn "\n--- Parametric Record Test ---"
    
    putStrLn "Shape with a String color:"
    print stringColorCircle
    
    putStrLn "\nShape with an RGB Tuple color:"
    print rgbColorRect
    
    putStrLn "\n--- Using the Shared Getter ---"
    putStrLn ("Circle's color value: " ++ color stringColorCircle)
    
    putStrLn ("Rectangle's color value: " ++ show (color rgbColorRect))
    
    putStrLn "\n--- Recursive Tweet Thread Test ---"
    
    putStrLn "Main Tweet (Notice the nested replies!):"
    print mainTweet
    
    putStrLn "\n--- Accessing nested data ---"
    putStrLn ("Number of direct replies: " ++ show (length (comments mainTweet)))
    
    putStrLn "\n--- Recursive Engagement Test ---"
    
    putStrLn "Calculating engagement for the top-level tweet..."
    
    let total = engagement topTweet
    
    putStrLn ("Total Thread Engagement: " ++ show total)
    
    putStrLn "\n--- Recursive Sequence Test ---"
    
    putStrLn "Our Number Sequence:"
    print myNumSeq
    
    putStrLn "\nOur String Sequence:"
    print myStringSeq
    
    putStrLn "\n--- Sequence Search Test ---"
    
    putStrLn "Is the number 2 in our sequence?"
    print (elemSeq 2 myNumSeq)      
    
    putStrLn "Is the number 99 in our sequence?"
    print (elemSeq 99 myNumSeq)     
    
    putStrLn "Is the word 'Haskell' in our string sequence?"
    print (elemSeq "Haskell" myStringSeq) 
    
    putStrLn "\n--- Binary Search Tree Test ---"
    
    putStrLn "Our BST in memory:"
    print myTree
