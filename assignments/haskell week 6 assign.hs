-- 1. Define a function `dropOdds :: Int -> Int` with the following behaviour. For any positive number `m`, `dropOdds m` is got by dropping all the odd digits in `m`. (If all the digits in the number are odd, the answer should be 0.)
-- Test cases:
-- dropOdds 0 = 0
-- dropOdds 8 = 8
-- dropOdds 1357 = 0

dropOdds :: Int -> Int
dropOdds n = if digits /= [] then foldr1 (\x y -> 10*y + x) digits else 0
    where
        digits = filter even (f (divMod n 10))
        f :: (Int, Int) -> [Int]
        f (0,r) = [r]
        f (q,r) = r : f (divMod q 10)

--------------------------------------------------------------------------------

-- 2. Define a function `moreZeros :: Int -> Bool` such that `moreZeros n` returns `True` exactly when the binary representation of `n` has strictly more 0s than 1s.
-- Test cases:
-- moreZeros 0 = True
-- moreZeros 1 = False
-- moreZeros 2 = False
-- moreZeros 4 = True

toBinary :: Int -> [Int]   -- [LSB ... MSB]
toBinary n = f (divMod n 2)
    where
        f (0,r) = [r]
        f (q,r) = r : f (divMod q 2)

moreZeros :: Int -> Bool
moreZeros n = let bin = toBinary n in length (filter (== 0) bin) > length (filter (== 1) bin)

--------------------------------------------------------------------------------

-- 3. Define a function `binToTer :: Int -> Int` which takes as input the binary representation of a number n and outputs the ternary representation of n. (You can assume that the input consists only of the digits 0 and 1, and the output should only consist the digits 0, 1 and 2.)
-- Test cases:
-- binToTer 0 = 0
-- binToTer 1 = 1
-- binToTer 11 = 10
-- binToTer 100 = 11

baseToDec :: Int -> Int -> Int
baseToDec b n = f (divMod n 10) 0
    where
        f (0,r) i = r*b^i
        f (q,r) i = r*b^i + f (divMod q 10) (i+1)

decToBase :: Int -> Int -> Int
decToBase b n = foldr1 (\x y -> 10*y + x) (f (divMod n b))
    where
        f (0,r) = [r]
        f (q,r) = r : f (divMod q b)

binToTer :: Int -> Int
binToTer = (decToBase 3) . (baseToDec 2)

--------------------------------------------------------------------------------

-- 4. Define a function `palindrome :: Int -> Bool` which outputs True exactly when the number is a palindrome (digits read from left to right is the same as digits read from right to left).
-- Test cases:
-- palindrome 0	= True
-- palindrome 121 = True

palindrome :: Int -> Bool
palindrome n = n == foldl1 (\x y -> 10*x + y) (f (divMod n 10))
    where
        f (0,r) = [r]
        f (q,r) = r : f (divMod q 10)
