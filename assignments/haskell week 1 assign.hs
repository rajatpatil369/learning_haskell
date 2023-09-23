-- 1. Type of the following funtion is?
f1 x y z = y == ((x+1 == 0) /= z)
-- ans
-- arithmetic operators have precedence over logical
f1 :: Int -> Bool -> Bool -> Bool

--------------------------------------------------------------------------------

-- 2. For how many pairs (x,y) does `f2 x y 3` return False, where -2 <= x <= 2 and -3 <= y <= 3?
f2 x y z = (x >= y) == not (0 <= z && z <= y)
-- ans
-- lets simplity funtion f2
f20 x y = (x >= y) == not (0 <= 3 && 3 <= y)
f21 x y = (x >= y) == not (3 <= y)
-- `a == not b` is equivalent to `a /= b`
f22 x y = (x >= y) /= (3 <= y)
-- side note: `a == b` is equivalent to XNOR, `a /= b` is equivalent to XOR
-- let's write a funtion to figre out for how many (x,y) pairs is the output True for funtion `f`
figureout :: (Int -> Int -> Bool) -> [Int] -> [Int] -> Int
figureout _ [] _ = 0
figureout func (x:xs) l = count func x l + figureout func xs l
    where
        count _ _ [] = 0
        count foo a (b:bs) = (if not (foo a b) then 1 else 0) + count foo a bs

f2wrapper a b = f2 a b 3
figureout f2wrapper [-2..2] [-3..3] -- returns 10
ghci> figureout f20 [-2..2] [-3..3] -- returns 10
ghci> figureout f21 [-2..2] [-3..3] -- returns 10
ghci> figureout f22 [-2..2] [-3..3] -- returns 10

--------------------------------------------------------------------------------

-- 3. What is the value of `f3 21 35`?
f3 a 0 = a
f3 a b
    | a >= b = f b (mod a b)
    | a < b = f b a
-- ans
-- looks like gcd, let's verify
-- f3 21 35
-- f3 35 21
-- f3 21 (mod 35 21)
-- f3 21 14
-- f3 14 (mod 35 14)
-- f3 14 7
-- f3 7 0
-- 7

--------------------------------------------------------------------------------

-- 4. What is the output of `f4 31 25`?
f4 x y
    | x <= 0 = 0
    | even x = f4 (x `div` 2) (y + y)
    | odd x = f4 (x `div` 2) (y + y) + y
-- ans
-- let's be lazy ;)
-- just keep in mind that invoking funtions have precidence over arithmetic opertions
-- 775

--------------------------------------------------------------------------------

-- 5. For how many triples (x,y,z) does `f5 x y z` evaluate to True?
f5 x y z = (not x || y) && (not y || z) && (z || x)
-- ans
-- a 3-input logic gate
-- using the above equation "Y = (A' + B).(B' + C).(C + A)" simplifies to "Y = C.(A' + B)"
-- okay okay... don't be angry... i didn't cheat

check func [] = 0
check func ((x,y,z):ips)
    | func x y z = 1 + check func ips
    | otherwise = check func ips

check f5 [(a,b,c) | a <- ip, b <- ip, c <- ip]  -- nPr = n!/(n-r)! = 3P3 = 8
    where
        ip = [True, False]
-- result is 3

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--- practice (non graded assignment)

-- 1. A possible type of the following funtion is?
f1 x y z = x || not (not y /= z)
-- ans
-- f1 :: Bool -> Bool -> Bool -> Bool

--------------------------------------------------------------------------------

-- 2. Consider the following incomplete definition of the NOR function.
f2 True _ = False
f2 _ y = ?
-- What should be in place of the question mark?
-- ans
-- a b y
-- 0 0 1
-- 0 1 0
-- 1 0 0
-- 1 1 0
-- first two cases are of our interest, its clear that `y = not b` when a=0
-- so, `not y` should be in place of ?

--------------------------------------------------------------------------------

-- 3. What is the value of `f3 5578`?
f3 n = g n 0
g n a
    | n == 0 = a
    | otherwise	= g q (10*a + r)
        where
            q = div n 10
            r = mod n 10
-- ans
-- f3 5578
-- g 557 8
-- g 55 87
-- g 5 875
-- g 0 8755
-- 8755
-- simple statergy to rever the digits of a number
-- chop off the remainder, multiply it by 10, add the remainder of quotient, repeat until the quotient itself becomes zero
