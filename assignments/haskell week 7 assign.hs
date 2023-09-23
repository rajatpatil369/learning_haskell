-- In this assignment you have to submit a standalone Haskell program that can be compiled using the ghc compiler. 
-- The input to the program will be multiple lines. Each input line is guaranteed to be a single word, with no beginning or trailing spaces. On reading a line, your program should print "Y" if the word is a palindrome (after converting every letter to lowercase), and "N" if not. 
-- A palindrome is a string that is the same as its reverse.
-- Here is a sample run:

-- Input
-- -----
-- abba
-- Level
-- Hi
-- Malayalam

-- Output
-- ------
-- Y
-- Y
-- N
-- Y

import Data.Array

isPalindrome :: String -> Bool
isPalindrome str = str == foldl (\x y -> y:x) "" str

toLowerCase :: String -> String
toLowerCase
    = let lut = listArray ('A','Z') ['a'..'z']
    in map (\c -> if 'a' <= c && c <= 'z' then c else lut!c)

main :: IO ()
main = do
    word <- getLine -- readLn :: IO String  -- the `readLn` "action" expects a valid representaion of the "type" you are typring to parse
    putStrLn (if isPalindrome (toLowerCase word) then "Y" else "N")
    main

{-
import Data.Char (toLower)

isPalindrome :: String -> Bool
isPalindrome str = cleanedStr == reverse cleanedStr
  where cleanedStr = map toLower str

main :: IO ()
main = do
  input <- getContents
  let linesOfInput = lines input
  let results = map (\line -> if isPalindrome line then "Y" else "N") linesOfInput
  mapM_ putStrLn results
-}
