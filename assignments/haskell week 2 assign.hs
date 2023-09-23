-- 1. What is the type of `[(&&), (<)]`?
-- ans
-- the answer (according to "them" is [Bool -> Bool -> Bool]
-- but i think it should be:
-- [(a -> a -> Bool)]
-- because type of `(&&)` is `Bool -> Bool -> Bool` and type of `(<)` is Ord a => a -> a -> Bool

--------------------------------------------------------------------------------

-- 2. What is the value of `l1 == l2`?
l1 = filter isUpper ['a'..'z']
l2 = zipWith (<) [0..26] [1..]
-- ans
isUpper ch = ch >= 'A' && ch <= 'Z'
-- `filter` will "filter" those `Char` that are upper case, so `l1` will be of type `[Char]`, the value of `l1` is `[]` or `""`
-- `zipWith` will take in two lists, and perform an element-wise operation on the elements of those lists sequentially until at least one of the lists gets exhausted
-- the type of `l2` will be `[Bool]`
-- so, `l1 == l2` will lead to a type error

--------------------------------------------------------------------------------

-- 3. What is the value of the expression `f3 [1..5]`?
f3 = g (\x -> x)
g k [] = k 100
g k (x:xs) = g ((x*) . k) xs
-- ans
-- in the whole course this is the first time i am seeing a lambda function and the dot operator (function composition)... did i missed a class?
-- f3 [1..5]
-- g (\x -> x) [1,2,3,4,5]
-- g ((1*).(\x->x)) [2,3,4,5]
-- g ((2*).((1*).(\x->x))) [3,4,5]
-- g ((3*).((2*).((1*).(\x->x)))) [4,5]
-- g ((4*).((3*).((2*).((1*).(\x->x))))) [5]
-- g ((5*).((4*).((3*).((2*).((1*).(\x->x)))))) []
-- ((5*).((4*).((3*).((2*).((1*).(\x->x)))))) 100
-- ((5*) ((4*) ((3*) ((2*) ((1*) ((\x->x) 100))))))
-- ((5*) ((4*) ((3*) ((2*) ((1*) 100)))))
-- ((5*) 2400)
-- 12000

--------------------------------------------------------------------------------

-- 4. What is the value of the expression `f4 6`?
f4 = snd . g
g 0 = (0,0)
g n =
    let (x,y) = g (n-1)
    in (x+1, y-x)
-- ans
-- i very much doubt that funtion composition and `let ... in ...` expression was covered in week 2's content
-- in ghci, `:doc snd` says that the funtion `snd` has a type of `snd :: (a, b)`-> b`, it extracts the SECOND component of a pair (tuple that strictly has 2 elements)
-- snd must stand for second, and not to your surprise there also exists `fst`
-- f4 6
-- (snd . g) 6
-- snd (g 6)
-- snd (let (x,y) = g 5 in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = g 4 in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = g 3 in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = g 2 in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = g 1 in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = g 0 in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (0, 0) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (1, 0) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (let (x,y) = (2, -1) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (let (x,y) = (3, -3) in (x+1, y-x)) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (let (x,y) = (4, -6) in (x+1, y-x)) in (x+1, y-x))
-- snd (let (x,y) = (5, -10) in (x+1, y-x))
-- snd (6, -15)
-- -15
-- woow... that was fun!
-- so... in short funtion `f4` is nothing but:
-- -(sum [1..(n-1)])            -- or
-- -(foldr (+) 0 [1..(n-1)])    -- so much for a simple thing huh...

--------------------------------------------------------------------------------

-- 5. Given below is an incomplete definition for the function `drop`. Complete the code by filling in the blank (the question mark).
mydrop n l
    | n <= 0 || null l = l
    | otherwise = ?
-- options
-- a. tail l
-- b. drop (n-1) (tail l)
-- c. drop (n-1) l
-- d. drop n (tail l) 
-- ans
-- `tail` returns everyting but the first element of a list
-- so `drop (n-1) (tail l)` would be equivalnet to `drop n l`

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--- practice (non graded assignment)

-- 1. What is the value of `l1 == l2`?
l1 = filter isUpper ['a'..'z']
l2 = [x | x <- [0,2..10], odd x]
-- ans
-- remember that the last element (10) is incleded in the A.P. (of course if the "d" permits)
-- since there're no upper case characters in `['a'..'z']` and similarly there are no odd integers in `[0,2..0]`
-- `l1` and `l2` would be empty (both!)
-- but... there's a twist!
-- we know that `[Char]` is a `String` and it is NOT equal to an empty list of `Int`
-- hence `l1 == l2` would result into a "type error", as we are comparing two values of different data types

--------------------------------------------------------------------------------

-- 2. What is the value of the function f defined below?
f = (l, length l)
    where l = [32, 28.3..1]
-- ans
-- in haskell, everything is a funtion, so don't scratch your head when the questioner calls the thing "f" that appently looks like a variable (a tuple)
-- a tuple is a non-homogenous collection
-- `[32, 28.3..1]` yields `[32.0,28.3,24.6,20.900000000000002,17.200000000000003,13.500000000000004,9.800000000000004,6.100000000000005,2.4000000000000057]`
-- so the value `f` is `([32,28.3,24.6,20.9,17.2,13.5,9.8,6.1,2.4],9)`
-- yeah... strip off any trailing zeros and intervening spaces (this obivously means that you were supposed to calculate the values by "hand", but atleast now you are aware of those zeros)

--------------------------------------------------------------------------------

-- 3. Given below is an incomplete definition for the function `take`. Complete the code by filling in the blank (the question mark).
mytake n l
    | n <= 0 || null l = []
    | otherwise = ?
-- ans
-- `[head l] ++ mytake (n-1) (tail l)`  -- or
-- `(head l):mytake (n-1) (tail l)`
