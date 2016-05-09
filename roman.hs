-- file: roman.hs
import Data.Char

-- Application startup.
main :: IO ()
main = do
    putStrLn "Enter a Roman numeral. The numerical value will be returned."
    putStrLn "For example MMI -> 2001."
    putStrLn "Enter 'finis' to exit."
    process
    putStrLn "vale! :-)"

-- Main loop. 
process :: IO ()
process = do
    putStrLn "Numeral: "
    line <- getLine
    if line == "finis"
        then return ()
        else do
            let nums = readNumerals(line)
                in if errorNum `elem` nums
                    then putStrLn (line ++ "is not a valid Roman numeral.")
                    else putStrLn ("   The Roman numeral " ++ line ++ " is equal to " ++ show(getValue(nums)))
            process
  
data Numeral = Numeral {
    numeralValue :: Int,
    numeralSymbol :: Char,
    subRule :: [Char]}
    deriving (Show, Eq)

-- This is the Numeral error type. used to check if the input was valid.
errorNum = Numeral 0 'E' []

-- Turn a string into a list of numerals.
readNumerals :: String -> [Numeral]
readNumerals (x:xs) = readNumeral (toUpper(x)) : [] ++ readNumerals xs -- First cons the Char to an empty list, then recur.
readNumerals "" = []

-- Parse each char into the corresponding numeral.
readNumeral :: Char -> Numeral
readNumeral 'I' = Numeral 1 'I' ['V', 'X']
readNumeral 'V' = Numeral 5 'V' []
readNumeral 'X' = Numeral 10 'X' ['L', 'C']
readNumeral 'L' = Numeral 50 'L' []
readNumeral 'C' = Numeral 100 'C' ['D', 'M']
readNumeral 'D' = Numeral 500 'D' []
readNumeral 'M' = Numeral 1000 'M' []
readNumeral _ = errorNum

-- Turn a list of Roman numerals into their corresponding Arabic number.
getValue :: [Numeral] -> Int
getValue [] = 0
getValue (x:xs) = 
                if subRuler (subRule x) xs
                then negate (numeralValue x) + getValue xs
                else numeralValue x + getValue xs
                -- if x has a non-empty subRule and the next x is in the list, then flip the sign for x
                    
-- Take the subRule from x and xs, if x of xs is in subRule, return true, otherwise false.
-- What this really means is see if the next element in the list of Numerals is one of the elements in subRule.
subRuler :: [Char] -> [Numeral] -> Bool
subRuler [] _ = False
subRuler _ [] = False
subRuler a b = numeralSymbol (head b) `elem` a

{-
Subtractive Rules, from Wikipedia
I placed before V or X indicates one less, so four is IV (one less than five) and nine is IX (one less than ten)
X placed before L or C indicates ten less, so forty is XL (ten less than fifty) and ninety is XC (ten less than a hundred)
C placed before D or M indicates a hundred less, so four hundred is CD (a hundred less than five hundred) and nine hundred is CM (a hundred less than a thousand)
-}
