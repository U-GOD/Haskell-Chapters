import GHC.Base (VecElem(Int16ElemRep))
import Control.Arrow (Arrow(first))
import Data.Char (toUpper)
import Distribution.Compat.CharParsing (CharParsing(string))

loopUntilQuit :: IO ()
loopUntilQuit = do
    putStrLn "\nType something (or type 'quit' to exit):"
    input <- getLine

    if input == "quit"
        then putStrLn "Exiting the loop. Goodbye!"
        else do
            putStrLn ("You typed: " ++ input)
            loopUntilQuit

wrapSumInIO :: Int -> Int -> IO Int
wrapSumInIO x y = do
    let total = x + y
    return total

main :: IO ()
main = do
    -- Task 1
    putStrLn "Hello! What is your name?"  
    name <- getLine
    putStrLn ("It is a pleasure to meet you, " ++ name ++ "!")

    -- Task2
    putStrLn "Please type a sentence, and i will count the characters:"
    inputString <- getLine
    print (length inputString)

    -- Task 3
    putStrLn "\nPlease enter a number to double:"
    numberString <- getLine
    let number = read numberString :: Int
    let doubled = number * 2
    putStrLn ("Double that number is: " ++ show doubled)

    -- Task 4
    putStrLn "\nPlease enter the first part of your sentense:"
    firstPart <- getLine
    putStrLn "Please enter the second part of your sentence:"
    secondPart <- getLine
    let combined = firstPart ++ secondPart
    putStrLn ("Concatenated result: " ++ combined)

    putStrLn "--- Repeat Until Quit Program---"
    loopUntilQuit
    putStrLn "The program has completely finished"

    putStrLn "Please enter a sentence to convert to uppercase:"
    inputString <- getLine

    let upperString = map toUpper inputString
    putStrLn ("Uppercase version: " ++ upperString)

    putStrLn "\n--- MAIN MENU ---"
    putStrLn "Greet the World"
    putStrLn "Perform a Calculation"
    putStrLn "Exit Program"
    putStrLn "Please select an option (1-3):"

    choice <- getLine
    case choice of
        "1" -> do
            putStrLn "Hello, World! It is a beautiful day."
        "2" -> do
            putStrLn "Calculating 100 * 5..."
            print (100 * 5)
        "3" -> do
            putStrLn "Goodbye! Shutting down..."
        _   -> do
            putStrLn "Invalid selection. Please try running the program again."

    putStrLn "\n--- Even or Odd Checker ---"
    putStrLn "Please enter a whole number:"
    numStr <- getLine
    let number = read numStr :: Int
    if even number
        then putStrLn (show number ++ " is an Even number.")
        else putStrLn (show number ++ " is an Odd number.")

    putStrLn"\n--- Sum Two Numbers ---"
    putStrLn "Please enter the first number:"
    firstStr <- getLine
    let num1 = read firstStr :: Int

    putStrLn "Please enter the second number:"
    secondStr <- getLine
    let num2 = read secondStr :: Int 

    finalSum <- wrapSumInIO num1 num2 
    putStrLn ("The sum is: " ++ show finalSum)

    putStrLn "\n--- Reverse User Input ---"
    putStrLn "Please enter a word or sentence to be reversed:"
    stringToReserve <- getLine
    let reversedString = reverse stringToReserve
    putStrLn ("Here is your reversed text: " ++ reversedString)