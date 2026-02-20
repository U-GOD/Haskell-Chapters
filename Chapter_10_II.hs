-- HC11T1 & HC11T2: WeAccept Class and Instances

-- 1. Define the Type Class (The Contract) ONCE
class WeAccept a where
    accepts :: a -> Bool

-- 2. HC11T1: The Box Data Type and Instance
data Box a = Empty | Has a deriving (Show, Eq)

instance WeAccept (Box a) where
    accepts Empty   = False
    accepts (Has _) = True

filterAcceptedBoxes :: [Box a] -> [Box a]
filterAcceptedBoxes boxes = filter accepts boxes

-- 3. HC11T2: The Diverse Data Types and Instances
data Cardano = Cardano deriving (Show)
data Cash    = Cash deriving (Show)
data Country = Country String deriving (Show)

instance WeAccept Cardano where
    accepts _ = True

instance WeAccept Cash where
    accepts _ = False

instance WeAccept Country where
    accepts (Country "Ghana") = True
    accepts _                 = False

fancyFunction :: WeAccept a => a -> String
fancyFunction item = 
    if accepts item 
    then "Fantastic! We gladly accept this."
    else "Apologies, this is not accepted here."

-- HC11T3: Container Type Class for Box
class Container c where
    isEmpty  :: c a -> Bool
    contains :: Eq a => a -> c a -> Bool
    replace  :: a -> c a -> c a

instance Container Box where
    isEmpty Empty   = True
    isEmpty (Has _) = False

    contains _ Empty        = False
    contains target (Has x) = target == x

    replace newVal _ = Has newVal

-- HC11T4: Container Instance for Present
data Present a = EmptyPresent | Wrapped a deriving (Show)

instance Container Present where
    isEmpty EmptyPresent = True
    isEmpty (Wrapped _)  = False

    contains _ EmptyPresent        = False
    contains target (Wrapped x)    = target == x

    replace newVal _ = Wrapped newVal

-- HC11T5: guessWhat'sInside Function for Containers

-- Define polymorphic function
-- "For any Container 'c' and any comparable item 'a', 
-- take a guess 'a', take the container 'c a', and return a Bool."
guessWhat'sInside :: (Container c, Eq a) => a -> c a -> Bool
guessWhat'sInside guess container = contains guess container

-- HC11T6: AdvancedEq for Blockchain
data Blockchain = Blockchain { chainLength :: Int } deriving (Show, Eq)

class Eq a => AdvancedEq a where
    compareEquality :: a -> a -> Bool

instance AdvancedEq Blockchain where
    compareEquality b1 b2 = b1 == b2

-- HC11T7: Ord Instance for Box
instance Ord a => Ord (Box a) where
    compare Empty Empty = EQ

    compare Empty (Has _) = LT

    compare (Has _) Empty = GT

    compare (Has x) (Has y) = compare x y

-- HC11T8: Deriving Eq and Ord for PaymentMethod
data PaymentMethod = PayCash | PayCard | PayCrypto deriving (Show, Eq, Ord)

data Length = M Double | Km Double deriving (Show, Eq)

instance Ord Length where
    compare (M val1) (M val2)   = compare val1 val2
    compare (Km val1) (Km val2) = compare val1 val2

    compare (M mVal) (Km kVal)  = compare mVal (kVal * 1000)
    compare (Km kVal) (M mVal)  = compare (kVal * 1000) mVal

-- HC11T10: sortContainers Function
sortContainers :: Ord a => [a] -> [a]
sortContainers [] = []
sortContainers (pivot:rest) = 
    let smaller = sortContainers [x | x <- rest, x <= pivot]
        bigger  = sortContainers [x | x <- rest, x >  pivot]
    in  smaller ++ [pivot] ++ bigger

main :: IO ()
main = do
    putStrLn "--- WeAccept Box Test ---"
    let shipment = [Has "Laptop", Empty, Has "Monitor", Has "Keyboard", Empty]
    
    putStrLn "Original Shipment:"
    print shipment
    
    putStrLn "\nFiltered Shipment (Accepted Boxes Only):"
    print (filterAcceptedBoxes shipment)
    
    putStrLn "\n--- Fancy Function Type Class Application Test ---"
    
    let myCrypto = Cardano
    let myWallet = Cash
    let local    = Country "Ghana"

    let otherCountry = Country "Atlantis"
    
    putStrLn "\nTrying to use Cardano:"
    putStrLn (fancyFunction myCrypto)
    
    putStrLn "\nTrying to use Cash:"
    putStrLn (fancyFunction myWallet)
    
    putStrLn "\nTrying to use Country 'Ghana':"
    putStrLn (fancyFunction local)
    
    putStrLn "\nTrying to use Country 'Atlantis':"
    putStrLn (fancyFunction otherCountry)
    
    putStrLn "\n--- Container Type Class Test ---"
    
    let myBox = Has 42
    let emptyBox = Empty :: Box Int
    
    putStrLn "Is myBox empty?"
    print (isEmpty myBox)
    
    putStrLn "\nIs emptyBox empty?"
    print (isEmpty emptyBox)
    
    putStrLn "\nDoes myBox contain 42?"
    print (contains 42 myBox)
    
    putStrLn "\nDoes myBox contain 99?"
    print (contains 99 myBox)
    
    putStrLn "\nReplacing myBox contents with 99:"
    print (replace 99 myBox)
    
    putStrLn "\nReplacing emptyBox contents with 100:"
    print (replace 100 emptyBox)
    
    putStrLn "\n--- Container Type Class Test for Present ---"
    
    let birthdayGift = Wrapped "Bicycle"
    let sadGift = EmptyPresent :: Present String
    
    putStrLn "Is the birthday gift empty?"
    print (isEmpty birthdayGift)
    
    putStrLn "\nDoes the birthday gift contain a 'Bicycle'?"
    print (contains "Bicycle" birthdayGift)
    
    putStrLn "\nReplacing the sad empty gift with a 'Book':"
    print (replace "Book" sadGift)
    
    putStrLn "\n--- guessWhat'sInside Function Test ---"
    
    let mysteryBox = Has "Golden Ticket"
    let mysteryPresent = Wrapped "Golden Ticket"
    
    putStrLn "Guessing if 'Apple' is in the Box:"
    print (guessWhat'sInside "Apple" mysteryBox)
    
    putStrLn "\nGuessing if 'Golden Ticket' is in the Box:"
    print (guessWhat'sInside "Golden Ticket" mysteryBox)
    
    putStrLn "\nGuessing if 'Golden Ticket' is in the Present:"
    print (guessWhat'sInside "Golden Ticket" mysteryPresent)
    
    putStrLn "\n--- AdvancedEq Subclass Test for Blockchain ---"
    
    let mainNet = Blockchain { chainLength = 5000 }
    let testNet = Blockchain { chainLength = 150 }
    let forkNet = Blockchain { chainLength = 5000 }
    
    putStrLn "Are mainNet and forkNet advanced-equal?"
    print (compareEquality mainNet forkNet)  
    
    putStrLn "Are mainNet and testNet advanced-equal?"
    print (compareEquality mainNet testNet)  
    
    putStrLn "\n--- Ord Instance for Box Test ---"
    
    let box10 = Has 10
    let box50 = Has 50
    let boxEmpty = Empty :: Box Int
    
    putStrLn "Comparing Empty to Has 10:"
    print (compare boxEmpty box10)
    
    putStrLn "\nComparing Has 50 to Has 10:"
    print (compare box50 box10)   
    
    putStrLn "\nComparing Has 10 to Has 10:"
    print (compare box10 (Has 10))  

    putStrLn "\nIs Has 10 less than Has 50?"
    print (box10 < box50) 
    
    putStrLn "\n--- Derived Eq and Ord Test for PaymentMethod ---"
    
    let payment1 = PayCash
    let payment2 = PayCrypto
    let payment3 = PayCard
    
    putStrLn "Is PayCash equal to PayCard? (Testing Eq)"
    print (payment1 == payment3) 
    
    putStrLn "\nIs PayCash less than PayCrypto? (Testing Ord)"
    print (payment1 < payment2)   
    
    putStrLn "\nWhich is greater: PayCard or PayCash? (Testing automatic 'max')"
    print (max payment3 payment1) 
    
    putStrLn "\n--- Length Data Type with Units Test ---"
    
    let track = M 400
    let road  = Km 1
    let marathon = Km 42.195
    let longWalk = M 2000
    
    putStrLn "Is a 400m track shorter than a 1km road?"
    print (track < road)        
    
    putStrLn "\nIs a 2000m walk greater than a 1km road?"
    print (longWalk > road)     
    
    putStrLn "\nComparing a 1km road to a 1000m track:"
    print (compare road (M 1000)) 
    
    putStrLn "\n--- sortContainers Function Test ---"
    
    -- Sorting manual Ord Boxes
    let mixedBoxes = [Has 50, Empty, Has 10, Has 99, Empty] :: [Box Int]
    
    putStrLn "Original Mixed Boxes:"
    print mixedBoxes
    
    putStrLn "\nSorted Boxes (Empty should come first, then numbers in order):"
    print (sortContainers mixedBoxes)
    
    -- Sorting derived Ord PaymentMethods
    let mixedPayments = [PayCrypto, PayCash, PayCard, PayCash]
    
    putStrLn "\nOriginal Mixed Payments:"
    print mixedPayments
    
    putStrLn "\nSorted Payments (Order of declaration: Cash -> Card -> Crypto):"
    print (sortContainers mixedPayments)
