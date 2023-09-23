-- this is a comment
-- https://www.haskell.org/ghcup/
-- curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
-- i installed everyting... don't know when what would be useful... ;)

-- docs and references:
-- https://downloads.haskell.org/ghc/latest/docs/users_guide/intro.html
-- https://wiki.haskell.org/Haskell_in_5_steps  -- seems old
-- https://wiki.haskell.org/Learn_Haskell_in_10_minutes
-- https://www.haskell.org/tutorial/
-- http://web.archive.org/web/20090306064337/http://darcs.haskell.org/yaht/yaht.pdf
-- https://learnyouahaskell.github.io/chapters.html -- looks up to date!
-- https://cabal.readthedocs.io/en/stable/getting-started.html

-- youtube:
-- https://www.youtube.com/watch?v=YNvFOCwS4tE&list=PLmTgnNwroyn8TnF26YRvW-hvQF1ypztzg
-- https://www.youtube.com/watch?v=pitjnqRKyyI&list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
-- https://www.youtube.com/watch?v=02_H3LjqMr8


cube :: Int -> Int -> Int   -- protoype or signature
cube x = x * x * x   -- definition

-- Int Float Char Bool
-- + - * / div mod
-- && || not
-- == /= > >= < <=
-- there are more operators, e.g. three versions of exponentiation operation: ^ ^^ **

-- write multiline commands within `:{` and `:}` in ghci

xor :: Bool -> Bool -> Bool
xor a b = (a && (not b)) || ((not a) && b)

inorder :: Int -> Int -> Int -> Bool
inorder x y z = (x <= y) && (y <= z)

-- pattern matching
xor2 :: Bool -> Bool -> Bool
xor2 True False = True  -- #top2bottom, 1st one match will execute
xor2 False True = True
xor2 a b = False

-- problematic definitions, do not use builtin functions' or keywords names
-- Prelude.or Main.or
myor :: Bool -> Bool -> Bool
myor True b = True
myor a True = True
myor a b = False

myand :: Bool -> Bool -> Bool
myand True b = b
myand False b = False
-- myand False _ = False
-- _ (which a wildcard pattern) denotes "don't care" argument, the value is not captured
and2 :: Bool -> Bool -> Bool
and2 True x = x
and2 False _ = False 

or2 :: Bool -> Bool -> Bool
or2 True _ = True
or2 True _ = True
or2 _ True = True
or2 _ _ = False -- since the functions has checked for the previous two conditions then its 100% confirm that BOTH of the arguments are False

-- another interesting brainteaser example
-- `nor a b` equals `True` only if both `a` and `b` are `True`
nor :: Bool -> Bool -> Bool
nor True _ = False
nor _ b = not b -- reaching this definition will mean `a` i.e. `_` is definitely `False`

-- recursive definitions
fact :: Int -> Int
fact 0 = 1
fact n = n * (fact (n - 1)) -- watchout for the brackets #Currying, functions consume arguments and split another function

-- conditional definition
factorial :: Int -> Int
factorial 0 = 1 -- base case
factorial n
    | n < 0 = factorial(-n) -- note the indentation
    | n > 0 = n * factorial (n - 1) -- each contitional is know as guard, guards need not to be mutually exclusive but #top2bottom

-- ghc = Glassgow Haskell Compiler
-- instrucctor says that functions cannot be define in the ghci (i = interpreter), but this doesn't seems to be true
-- use :load (or :l for short) to load .hs files

-- problem: if all the function definitions or guards are exhausted, then the interpreter will throw some sorts of pattern matching error
-- solution: otherwise clasue (which ascts as "else" in "if-else" or as "Catch All") makes sure that no error occurs if all cases or guards are not specified
facto :: Int -> Int
facto n
    | n == 0 = 1    -- base case moved here
    | n > 0 = n * factorial (n - 1)
    | otherwise = facto(-n) -- will evaluate for every other condition
-- otherwise acts as a "catch all phrase", it ensures we don't have any pattern match failures

-- person who invented haskell = moses schonfinkel
-- we write the arguments/parameters in a sequence seperated by spaces rather than the conventional notation where the arguments are enclosed in parenthesis and seperated by comma
-- fun fact: haskell currying = <language_name> <curry> = logocian who made this "not so conventional" notation popular
-- reason: instead of multiple arguments, you get a sequence of functions, which consume one argument at a time, each argument transforms the function into a new one internalizing the new consumed argument
-- hence the arrow notation: ->
-- simplified: Int -> Int -> Int -> Bool
-- actual: (Int -> (Int -> (Int -> Bool)))
-- likewise, function application brackets from left, i.e. it's left associative
-- ((((f x1) x2) x3) ...) xn

-- gcd = greatest common diviser = highest common factor, if gcd = 1, numbers are at least co-prime
gcd :: Int -> Int -> Int
gcd m 0 = m
gcd 0 n = n
gcd m n
    | m == n = m
    | m > n = gcd n (mod m n)
    | m < n = gcd m (mod n m)

-- sir's approach
gcd2 :: Int -> Int -> Int   -- the classic version
gcd2 a 0 = a
acd2 a b
    | a >= b = gcd b (mod a b)
    | otherwise = gcd b a

gcd3 :: Int -> Int -> Int   -- the "slow" version
gcd3 a b
    | a == 0 || b == 0 = a + b
    | a <= b = gcd a (b-a)
    | a > b = gcd b (a-b)

-- largest divisor of n less than n
-- strategy: try n-1, n-2, ... in the worst case, stop at 1
lar_div :: Int -> Int
lar_div n = div_ser n (n-1) -- watchout! `div_ser n n-1` means `(div_ser n n) - 1` and NOT `div_ser n (n-1)`... #Currying

div_ser :: Int -> Int -> INT
-- div_ser x 1 = 1 -- don't know if pattern matching is expensive or falling down till `mod` guard is...
div_ser x i -- implementation of a loop
    | i >= x = x -- not required... just being explicit here
    | mod x i == 0 = i
    | otherwise = div_ser x (i-1)   -- brackets!!! #Currying

-- y = log(x, base)
-- x = base^y
-- integer log:
-- how many times (i.e. `y`) can `base` be multiplied in order to get `x`
-- how many times can `x` be divided by `base` till we reach 1 i.e. [1, 2)

int_log :: Int -> Int -> Int
int_log 1 _ = 0 -- any number to the power 1 is 0
int_log x b
    | x >= b = 1 + int_log (div x b) b  -- note the parenthesis!
    | otherwise = 0

-- IMPORTANT: ALWAYS PUT PARENTHESIS WHEN SPECIFYING NEGATIVE NUMBERS

-- reverse digits of a number
rev :: Int -> Int
rev n 
    | n < 10 = n
    | otherwise = mod n 10 * 10 ^ cou_dig (div n 10) + rev (div n 10)
-- note the use of parenthesis
-- function application is left-associative, therefore, `cou_dig div n 10` will be interpreted as `(cou_dig div) n 10` and as hsakell desn't know how to "treat" the result of `(succ div)` with arguments `n 10` it throws an error

cou_dig :: Int -> Int
cou_dig n
    | n < 10 = 1
    | otherwise = 1 + cou_dig (div n 10)

-- sir's approach
int_rev :: Int -> Int
int_rev n
    | n < 10 = n
    | otherwise = (int_rev (div n 10)) + (mod n 10) * (pow 10 (int_log n 10))

pow :: Int -> Int -> Int
pow m 0 = 1 -- m^n = m*m^(n-1)
pow m n = m * (pow m (n-1))


-- Q1.2 For how many pairs (x,y) does f x y 3 return False, where -2 <= x <= 2 and -3 <= y <= 3?
f :: Int -> Int -> Int -> Bool
f x y z = (x >= y) == not (0 <= z && z <= y)
-- ans: the following can be considered an example of nested looping

start :: [Int] -> [Int] -> Int
start [] _ = 0
start (x:xs) y = (process x y) + (start xs y)

process :: Int -> [Int] -> Int
process _ [] = 0
process a (b:bs) = (if not (f a b 3) then 1 else 0) + (process a bs)

start [-2..2] [-3..3] -- be carefull with negative numbers!!!


-- collective datatype = list, sequence of like elements
-- [T], where T is a data type
-- an empty list [] can denote any type
-- e.g. [[Int]] = [[1,2,3],[5],[6,7], []], a list of integer list 
-- a list MUST contain homogeneous elements (of uniform data type)
-- the colon operator : is appends an element to the left of a list
-- 1:[2,3] evaluates to [1,2,3]
-- internal representation or interpretation = 1:(2:(3:[])) i.e. : is right associative, similarly as a function data type is, and countrary to function application
-- `head (x:xs)` returns `x`    both of these functions
-- `tail (x:xs)` returns `xs`   are defined for non-empty list
-- (x:xs) can be consider as an unpacking the list in to (<fist_element>:<rest_of_the_list>
-- where, rest_of_the_list can be empty (if the original list contains only one leement), but will never be empty
-- (x:xs) on an empty list results into an error

-- fun fact: all the "in-built" functions are in a module called Prelude which gets loaded automatically (which can be prevented as well)

-- functions that work on a list (e.g. `f (x:xs)`) are defined in terms of `x f xs`

-- example: compute the length of a list
len :: [Int] -> Int
len [] = 0
len l = 1 + len (tail l)
-- len (x:xs) = 1 + len xs

-- example: sum of elements of a list
mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + sum xs  -- again! notice the parenthesis!
-- function application has a tighter binding, it has precedence over arithmetic

-- subscripting operator []
-- to access the i th element with in a list l use l[i]
-- [1,2,3] is only a short form of 1:(2:(3:[]))
-- to in order to access the i th element, you have to "peel-of" element intil you reach i th postition (need to peel-off j applications of : operator)
-- accesing i th value takes time proportional to i (unlike an array which has a constant time random access operation as it's contigous in memory, the memory location can be computed using the base value, offset and size of homogeneous data type)

-- list notation
-- `[m..n]` gives `[m,m+1,m+2,...,n-1,n]`, note that `n` is inclusive
-- or empty list if `m < n`
-- arith metic progetions = {a,a+d,a+2d,a+3d,...,n} = [a-d, a..l]
-- e.g. `[2,5..19]` gives `[2,5,8,11,14,17]`
-- about the `a-d` term: in the above e.g. 5-2=3, therefore d=3
-- it's not neceesary that `l` should be a part of the progrssion, nearest value less than or equal to `l` that is a part of the progrssion will be included
-- e.g. `[8,7..5]` gives `[8,7,6,5]`
-- `['a'..'z']` gives `"abcdefghijklmnopqrstuvwxyz"`
-- overall, you can think of this link: provide the fist two terms of the AP, then rest will be computed till last spefied element
-- i.e. [a,a+d..l]
-- this is also known as the range notation

-- examples: functions on lists
appendr :: [Int] -> Int -> [Int]
appendr [] e = [e]
appendr (x:xs) e = x:(appendr xs e) -- remember the internal representation of list?

-- IMPORTANT NOTE: whatever you think of in haskell think of it as an expanding recursive function... (much more like a macro or compiler direcitives in C) until a base case is met... every thing is expresses as a function or pure math


-- fusing two lists together
attach :: [Int] -> [Int] -> [Int]
attach l [] = l
attach l (x:xs) = attach (appendr l x) xs

-- sir's approch
atch :: [Int] -> [Int] -> [Int]
atch [] l = l
atch (x:xs) l = x:(atch xs l)   -- pull out `x`, attach `xs` and `l`, then stick back `x`

-- Builtin operator ++
-- `[3,2]++[4,6,7]` yeilds `[3,2,4,6,7]`

-- example: reversing a list
revl :: [Int] -> [Int]
revl [x] = [x]  -- instead of this:
-- revl [] = [] -- sir's approach
-- problem in my `[x]` is that it will be non-exhaustive, i.e. revl [] will have no match leading to an exception
revl (x:xs) = (revl xs) ++ [x]
-- revl (x:xs) = appendr (revl xs) x

-- examplee: is a list sorted
asc :: [Int] -> Bool
asc [] = True
asc [_] = True
-- similar to unpacking arguments in python = decomposing
asc (x:y:xs) = x <= y && asc (y:xs) -- `asc y:xs` will be interpreted as (asc y):xs
-- note the two level pattern (x:y:xs)

-- example: check if a list in integers is alternatiing = updown or downup
alt :: [Int] -> Bool
alt l = (ud l) || (du l)

ud :: [Int] -> Bool
ud [] = True
ud [x] = True
ud (x:y:xs) = (x < y) && (du (y:xs))

du :: [Int] -> Bool
du [] = True
du [x] = True
du (x:y:xs) = (x > y) && (ud (y:xs))

-- some builtin functions that work on list: head, tail, length, sum, reverse, init, last, take, drop

-- example: return all elements but last
myinit :: [Int] -> [Int]
myinit [_] = []
myinit (x:xs) = x:init xs   -- no brackets needed, since : operator is right associative

-- example: return last element of the list
mylast :: [Int] -> Int
mylast [x] = x
mylast (x:xs) = last xs
-- `myinit` and `mylast` will only work list with atleast one element

-- l == (take n l) ++ (drop n l)

-- example: return first n values in a list
mytake :: [Int] -> Int -> [Int]
mytake [] _ = []
mytake l n 
    | n <= 0 = []
    | n >= len l = l    -- this is also covered by the base case = redundant
    | otherwise = (head l):(mytake (tail l) (n-1))

-- sir's approach = doesn't involve head, tail, or len
mytake2 :: [Int] -> Int -> [Int]
mytake2 [] _ = []
mytake2 (x:xs) n
    | n <= 0 = []
    | otherwise = x:(mytake2 xs (n-1))

-- example: drop first n values in a list
mydrop :: [Int] -> Int -> [Int]
mydrop l 0 = l
mydrop l n
    | n <= 0 = l
    | n >= len l = []
    | otherwise = mydrop (tail l) (n-1)

mydrop2 :: [Int] -> Int -> [Int]
mydrop2 [] _ = []
mydrop2 (x:xs) n
    | n <= 0 = x:xs
    | otherwise = mydrop2 xs (n-1)  

-- example: point to ponder... we defined len function for `[Int]` can this be more generic, like [T]? since type T is irrelevant in the context of len function


-- ascii = 1 byte representation of characters
-- characters = 'a', 'b', '\r', '!', ':', ...
-- look up tabel of ascii code and character
-- python equivalent `ord` and `chr` exists in Data.Char
-- use `import` statement to import `ord` and `chr` functions' definitions
import Data.Char
'r' == chr (ord 'r')    -- True
114 == ord (chr 114)    -- True
-- `ord` and `chr` connect characters and ascii table

-- example: capitalize a character
cap :: Char -> Char
cap c
    | c >= 'a' && c <= 'z' = chr (ord c - 32)   -- (ord 'a' - ord 'A') = 32
    | otherwise = c
-- another approach might involve covering all cases of lower case charaters exhausetively and returning their upper case version, otherwise the same

-- a string is a sequence of characters
-- `String` = synonym for `[Char]`
-- e.g. `['h', 'e', 'l', 'l', 'o'] == "hello"` is True
-- `"" == []` is True, `'' == []` is NOT -> error
-- many list functions can be used on strings: tail, head, take, drop, length, reverse, init, last

-- example: return True if a character occurs with in a string
occ :: String -> Char -> Bool
occ "" _ = False
occ (x:xs) c = x == c || occ xs c
-- we know that haskell is lazy... so may be if in logical or operation if the first argument is true then the rest is not even evaluated???

-- sir's approach
occ2 :: String -> Char -> Bool
occ2 "" _ = False
occ2 (x:xs) c
    | x == c = True
    | otherwise = occ2 xs c

-- example: convert entire string to uppercase
upp :: String -> String
upp "" = ""
upp (x:xs) = (cap x):(upp xs)
-- this is a classic example of "applying a function `f` to all elements of a list `l` and producing a new one as a result"

-- example: first position of a character in a string, length otherwise
pos :: String -> Char -> Int
pos "" _ = 0
pos (x:xs) ch
    | x == ch = 0
    | otherwise = 1 + pos xs ch

-- example: find the number or words in a string (words are seperated by space, tab or new line character)
wor :: String -> Int
wor str = (cou_ch str ' ' + cou_ch str '\t' + cou_ch str '\n') + 1

-- helper function to cunter number of times a character occus in a string
cou_ch :: String -> Char -> Int
cou_ch "" _ = 0
cou_ch (x:xs) c
    | x == c = 1 + cou_ch xs c
    | otherwise = cou_ch xs c

-- sir's approch
wscount :: String -> Int
wscount "" = 0
wscount (c:cs)
    | whitespace c = 1 + wscount cs
    | otherwise = wscount cs
-- not enough! consider "abc   d" -> only one white space but gives 3

-- sir's approach, auxilliary function
wordcaux :: String -> Int
wordcaux [c] = 0
wordcaux (x:y:xs)
    | is_whitespace x && not (is_whitespace y) = 1 + wordcaux (y:xs)
    | otherwise = wordcaux (y:xs)

-- helper function
is_whitespace :: Char -> Bool
is_whitespace ' ' = True
is_whitespace '\t' = True
is_whitespace '\n' = True
is_whitespace _ = False

wordc :: String -> Int
wordc str = wordcaux (' ':str)
-- the space ensures that the 1st word is captured and...
-- error due to wordcaux "xx" (string of length 2) never raises, so the auxiliary function is not ment to be used independently !!! THIS IS NOT TRUE, because in case of "xx" xs will be []

-- हॊबासक्या नाहि मारायच्या... काम हॊत आहॆ कि नाहि फक्त तॆच पहायच...
-- जास्त universal function किव्वा गॊष्टी perfection नॆ करण्याच्या मागॆ नाहि लागायच...

-- list are only of uniform type.. tuples to the rescue
-- `(2,-21)::(Int,Int)`
-- `([1,3],True,73)::([Int],Bool,Int)`
-- a tuple type (T1, T2, ..., Tn) groups together multiple types

-- example: sum pairs of integers in a list of pairs
sumpairs :: [(Int, Int)] -> Int
sumpairs [] = 0
sumpairs (x,y):zs = x + y + sumpairs zs

-- example: find the marks of a specific student
lookup :: String -> [(String,Int)] -> Int
lookup _ [] = -1
lookup stu ((name,marks):mls)    -- `lookup stu (name,marks):ms` will be interpretes as `(lookup stu (name,marks)):ms`
    | p == name = marks
    | otherwise = lookup stu mls
-- `(name,marks)` this is tuple matching
-- a tuple glues elements of different data types together and creates a totally new data type altogether??? probably...? reason: type aliases, this is NOT true... type aliases are just different names for two same things 

-- sometimes it might becomes tedious to write `[(String,Int)]` mutiple times
-- or the type is more complex
type Marklist = [(String,Int)]  -- this is just a synonym, note that we are not making a new data type here
lookup2 :: String -> Marklist -> Int
lookup2 stu ml = lookup stu ml

-- example: calculate euclidience distance between two points in a two dimentional space
type Point2D = (Float,Float)

distance :: Point2D -> Point2D -> Float
distance (x1,y1) (x2,y2) = sqrt ((x2-x1)^2 + (y2-y2)^2)
-- notice the tuple pattern matching going on

-- local definitions
-- for 3D points
type Point3D = (Float,Float,Float)
dis :: Point3D -> Point3D -> Float
dis (x1,y1,z1) (x2,y2,z2) = sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(z2-z1) + (z2-z1)*(z2-z1))
-- yes... yes... we have the exponentiation operator, just bare with me

-- here we can make the tedious expression simplified by definit the `sqr` function
-- this would be much more legible (readable) approch of writting code

sqr :: Float -> Float
sqr x = x*x

dis2 :: Point3D -> Point3D -> Float
dis2 (x1,y1,z1) (x2,y2,z2) = sqrt (sqr (x2-x1) + sqr (y2-y1) + sqr (z2-z1))

-- now here, we have defined `sqr` function to make our life easier
-- doing this exposed the `sqr` function to the whole script and others
-- remember we defined an auxilliary function as `wordcaux`?
-- but this not be what we want... we might want to localize the aux. function 
dis3 :: Point3D -> Point3D -> Float
dis3 (x1,y1,z1) (x2,y2,z2) = sqrt (sqr xdiff + sqr ydiff + sqr zdiff)
    where
        xdiff :: Float
        xdiff = x2-x1
        ydiff :: Float
        ydiff = y2-y1
        zdiff :: Float
        zdiff = z2-z1
        sqr :: Float -> Float
        sqr x = x*x
-- `xdiff` and `ydiff` are another motivation for "local definitions"

-- point to be noted:
-- let's say you have written `Xdiff*Xdiff` in place of `sqr Xdiff` then,
-- `Xdiff*Xdiff` would be more efficient than `(X2-X1)*(X2-X1)`,
-- since in`Xdiff*Xdiff` `X2-X1` will be calculated only once

-- hence we should use `where` which allows local definitions to ensure common subexpressions are evaluated only once, and avoid wasteful computations

-- BRAIN TEASERS
-- Q1. What is the possible type of function `f1`?
f1 x y z = x || not (not y /= z)
-- it will be `Bool -> Bool -> Bool -> Bool`
-- since the expresion `x || not (not y /= z)` is like:
-- `x || not ((not y) /= z)` and NOT `x || not (not (y /= z))`
-- and since that's true, `y` must be a `Bool` in order to be used with `not`,
-- `z` also must be a `Bool` since it's being compared with a `Bool`,
-- and `x` is apparently `Bool` as it's being used with || operator

-- Q2. What is the value of `f 5578`?
f n = g n 0
g n a
    | n == 0 = a
    | otherwise	= g q (10*a + r)
        where
            q = div n 10
            r = mod n 10
-- you may say that `f` acts as a wrapper function for `g`
-- this is a `rever the digits of an integer` function
-- if you look closely, in each recursive iteration,
-- the number `n` is getting chopper off into quotien and remainder when divided by 10,
-- and then as the process continues, in next iteration,
-- the remainder gets multiplied with 10 and result is added with the remainder of the result obtained by dividing previously obtained quotient with 10
-- this process continues till the original number becomes zero, and at that point we wouuld have effectively reverse the int


-- computation as rewritting
-- definitions allow us to replace expressions on lhs with rhs
-- process of replacing lhs by rhs is called reduction
-- when an expression can no no longer be reduced, it means we have computed an answer

-- consider the following definition of power function we defined earlier
-- pow :: Int -> Int -> Int
-- pow m 0 = 1 -- m^n = m*m^(n-1)
-- pow m n = m * (pow m (n-1))

-- essentially, it says that replace or substitute `pow m n` with `m * (pow m (n-1))` till we get `pow _ 0` (which equates to 1)
-- so that we will get an expresion that can be evaluated
-- here are the steps involved
-- e.g.
-- pow 3 2
-- 3 * (pow 3 (2-1))
-- 3 * (pow 3 1)
-- 3 * (3 * (pow 3 (1-1)))
-- 3 * (3 * (pow 3 0))
-- 3 * (3 * (1))
-- 3 * (3 * 1)
-- 3 * 3
-- 9    -- this is how we reach to the conclusion that "3 to the power 2 is 9"
-- # computation as rewriting

-- order of evaluation
-- (8+3)*(5-3) -> 11*(5-3) -> 11*2 -> 22    -- if we chose to left to right
-- (8+3)*(5-3) -> (8+3)*2 -> 11*2 -> 22    -- if we chose to right to left
-- this l2r or r2l won;t matter in case or arithmetics
-- but... conside the following
-- pow (5+2) (4-4) -> pow 7 (4-4) -> pow 7 0 -> 1   -- l2r
-- pow (5+2) (4-4) -> pow (5-2) 0 -> 1   -- r2l
-- note how the step of "pow 7 0" was skipped in r2l
-- but...
-- what "pow (div 3 0) 0" will return?
-- should it bindly follow anything to the power 0 is 1
-- or throw a division_by_zero error?

-- lazy evaluation
-- evalution of expression is done by haskell keeping in mind that minimum possible work should be done to figure out the answer
-- e.g. `head (2:reverse [1..5])`
-- in this example haskell will evaluate the outermost parenthesis and will directly give out the answer as two rather than evaluting in to out... so `reverse [1..5]` never gets evaluated in the first place
-- but in case of `last (2:reverse [1..5])`, `reverse [1..5]` must be evaluated to conclude that 5 is the answer
-- haskell evalutes from out to in, if it thinks that it has sufficient data to determine the answer then it won't evaluate futher

-- so coming back to "pow (div 3 0) 0"...
-- this will actually evaluate to 1 and won't throw any error

-- lazy evaluation is also called as "call by need"
-- computations can terminate even though there are undefined sub expressions

-- haskell allows us to define infinite lists as `[n..]`
-- so...
-- head ([n..])
-- head (n:[n+1..])
-- n

-- take 2 [n..]
-- n:(take 1 [n+1..])
-- n:(n+1:(take 0 [n+2..]))
-- [n,n+1]


-- polymorphosm in haskell
-- usage: remember we defined a len function that computed the length of a list of integers?
-- in this case limiting the input datatype to to just `[Int]` does make sense...
-- so, the question arises can we define a more generic version of `len` function that will accter the list of any type T and compute its length?
-- polymorphism to the rescue...
-- use type variables (e.g. a, b, c, ...) as place holders for types
-- e.g. `[a]` is a list of type `a`
-- meaning, the type of elements MUST be uniform throughout the list

len2 :: [a] -> Int
len2 [] = 0
len2 (x:xs) = 1 + len2 xs

-- similarly:
revl2 :: [a] -> [a]
revl2 [] = []
revl2 (x:xs) = (revl2 xs):x

myhead :: [a] -> [a]
myhead (x:xs) = x   -- the input list MUST have atleast one element


-- functions and operators
-- ususally we write operators like + - * / && || in infix notation
-- div mod are functions used in prefix notation
-- if you want to switch between these two:
-- use operators as functions by enclusing the operator into parenthesis
-- (+) 3 5
-- (++) [1,2] [3,4]
-- (-) 11 7
-- (&&) True True
-- and the other way around for functions is:
-- 7 `div` 5
-- 11 `mod` 3

-- consider the following
plus :: Int -> Int -> Int
plus a b = a + b
-- we know from currying that
-- (plus m) :: Int -> Int
-- ((plus m) n) :: Int
-- so likewise:
-- (+) m n
-- (((+) m) n)
-- this unlock a new feature: partial functions!
-- where one argument is fix and you can applya the new function that is formed by internalizing the constant argument to another argument


-- in haskell functions can be passed as arguments
apply f x = f x
-- here let f be a genric function that has a type `f :: a -> b`
-- where a and b are not constrained to any types
-- then apply will have a signature of:
-- apply :: (a, b) -> a -> b

-- application of these types of "higher order functions" (i.e. functions that take functions as argument) is sorting
-- in sorting we usually provide a key argument which is a function that determines the sorting criteria like asc or des
-- filtering is another classic example

-- applying functions to list
-- consider the following functions that apply a specific operation to all the elments in a list
sqrl :: [Int] -> [Int]
sqrl [] = []
sqrl (x:xs) = (x^2):(sqrl xs)

toupp :: String -> String
toupp "" = ""
toupp (c:cs) = (cap c):(toupp cs)

-- this is exactly what the `map` functions does, it takes in a function `f` and a list `l`, and "maps" each `x` to `f x` within the list `l`
-- map f [x0,x1,...,xn] => [f x0,f x1,...,f xn]
-- the equivalent of above functions would be:
map (^2) [1,2,3,4]
map cap ['a'..'z']
-- note the  partial function `(^2)`
-- examples
map (+3) [2,6,8]    -- [5,9,11]
map (3*) [2,6,8]    -- same as `map (3*) [2,6,8]`
-- but `map (2^) [1,2,3,4]` is NOT same as `map (^2) [1,2,3,4]`
map chr [0..127]    -- list of all charaters with ascii code range [0,127]

-- example: given a list of lists, sum the lengths of inner lists
-- traditional approach
suml :: [[a]] -> Int
suml [] = 0
suml (x:xs) = len2 x + suml xs

-- using `map`
suml2 :: [[a]] -> Int
suml2 l = sum (map len l)

-- definition of `map`, yes map takes strictly a list of `a` not tuple
map2 :: (a -> b) -> [a] -> [b]
map _ [] = []
map2 f (x:xs) = (f x):(map f xs)
-- place very close attentention to the type of the funtion `f`
-- `f` takes input of type `a` and produces output of type `b`
-- and hence the 2nd argument of the `map` func. is list of `a` and output is list of type `b`

-- filtering even numbers from a list of integers
evenonly :: [Int] -> Int
evenonly [] = []
evenonly (x:xs)
    | iseven x = x:(evenonly xs)
    | otherwise = evenonly xs
    where
        iseven :: Int -> Bool
        iseven n = mod n 2 == 0

-- filter selects all items from list `l` that satisfies a property `p`
fil :: (a -> Bool) -> [a] -> [a]    -- it may very well be possible that `[a]` is empty
fil _ [] = []
fil p (x:xs)
    | p x = x:(fil p xs)
    | otherwise = fil p xs
-- notice the type of `fil`
-- a funtion `f` takes an argument of type `a` and transsforms it into a result of type `b`
-- a property `p` takes an element of type `a` and tell us whether it satisfies a property or not
-- ultimately a property is a funtion that takes an argument and gives a `Bool`

-- a much more succinct way of writting `evenonly`
evenonly2 :: [Int] -> [Int]
evenonly2 l = fil is_even l
    where
        is_even :: Int -> Bool
        is_even n = mod n 2 == 0

-- combining `map` and `filter`, this is done very often
cap_vow :: String -> String
cap_vow l = map cap (filter is_vow l)

is_vow :: Char -> Bool
is_vow 'a' = True
is_vow 'e' = True
is_vow 'i' = True
is_vow 'o' = True
is_vow 'u' = True
is_vow _ = False

-- example: sqaring even numbers only

sqr_even :: [Int] -> [Int]
sqr_even l = map (^2) (filter even l)

-- square of first for EVEN integers
take 5 (sqr_even [0..])


-- list comprehension = set builder notation (set comprehension)
-- e.g.
-- M = { x² | x ∈ L even(x) }
-- this example says that,
-- M is a set of square of x, xuch that x belongs to (oe is an element of) set L, that satisfies the property of being even
-- in haskell this would be: (let L = {0,1,2,3,4,5,6,7,8,9}, i.e. the set of first 10 whole numbers)
[x^2 | x <- [0..9], even x]
-- this is almost verbatim to the mathematical equivalent we saw above
-- notice how the {} changes to [], ∈ changes to <-, and "such that" remains same as "|", AND DON'T FORGET THE COMMA
-- an important observation here would be that list comprehension is a combination of `map` and `filter`
-- `x^2` is the mapping, `even x` is the filtering

-- example: divisors of n, in descendding order
divisors :: Int -> [Int]
divisors n = [x | x <- [n,n-1..1], (mod n x) == 0]
-- `divisors 1` gives `[1]`

-- example: primes below n
primes :: Int -> [Int]
primes n = [x | x <- [1..n], divisors x == [x,1]]

-- multiple generators
-- example: a list of tuple of two numbers, such that their sum equals 10
[(x,y) | x <- [1..10], y <- [10,9..1], x + y == 10]
-- this: `[(x,y) | x <- [1..10], y <- [10,9..1]'`
-- is equivalent to (python): `[(x,y) for x in range (1,11) for y in range(10,0,-1)]`
-- i.e. `[(1,10),(1,9),...,(1,2),(1,1), (2,10),...,(2,1), ... ,(10,10),...,(10,1)]`
-- like nested loops, later generators move faster
-- generator(s) to the right run for EACH element of the generator(s) the left

-- example: pythagorian triplets below 100
[(a,b,c) | a<-[1..100], b<-[1..100], c<-[1..100], c^2 == a^2 + b^2]
-- wooh... that seems computentionally very intensive... took some time to finish
-- permutations! 100P1 * 100P1 * 100P1 = 10,00,000
-- one catche here is that it produces duplicates like (3,4,5) and (4,3,5)... since permutations

-- a clever way to tackle this would be to:
[(a,b,c) | a<-[1..100], b<-[a+1..100], c<-[b+1..100], c^2 == a^2 + b^2]
-- in the example of duplicate `(3,4,5)` and `(4,3,5)`
-- if the range of `b` is [a+1,100] then its guaranteed that 3 will never be genrated after a 4

-- tip: use `:doc <name>` in the ghci to find documentation for a funtion... this feature is experimental

-- example: concatinate lists with in a list
cnct :: [[a]] -> [a]
cnct l :: [x | y <- l, x <- y]
-- this is like (python): `[j for i in [[1,2,3],[4],[5,6]] for j in i]`
-- bultin funtion `concat`

-- extract all the even length non empty list
[x | x <- [[1,2,3],[4],[5,6],[]], length x /= 0 && mod (length x) 2 == 0]
-- another way could to do pattern matching within a list comprehension
-- the pattern `(x:xs)` won't match for an empty list
[(x:xs) | (x:xs) <- [[1,2,3],[4],[5,6],[]], mod (length (x:xs)) 2 == 0]

-- example: extract the head of non-empty even lengthed lists
[x | (x:xs) <- [[1,2,3],[4],[5,6],[]], mod (length (x:xs)) 2 == 0]

-- translating list comprehensions
-- LC are just a part of haskell's "syntactic sugar suite"
-- they are not a fundamental concept, rather just a means of writing more readable nested compositions of `map`, `filter` and `concat`
-- general form of LC
-- [e | q1, q2, ..., qN]
-- where `qj` is either:
--  a boolean condition `b`" or 
--  a generator `p <- l`" where `p` is a pattern and `l` is a list valued expression

-- in line if-else, another valid syntax
fac n = if n == 0 then 1 else n * fac (n-1)

-- in LC a boolean condition `b` acts as a filter, take a look at the following
-- [e | b, Q] = if b then [e | Q] else []
-- depends only on genrators/qualifiers to its left

-- a generator `P <- l` produces a list of candidates
-- naive translation:
-- [e | p <- l, Q] = map f l
-- where
--     f p = [e | Q]
--     f _ = []

-- note that if you are putting condition in front of a generator, i.e. like [x | <condition> <generator>] then you cannot use the variables after the condition

-- example: consider square of all even numbers from 1 to 7
[n*n | n <- [1..7], mod n 2 == 0]
-- above expression gives: `[4,16,36]`

-- translation
wrapper = map f [1..7]
    where
        f n = [n*n | mod n 2 == 0]  -- return `[n*n]` if `n` is even, `[]` otherwise
-- or
wrapper = map f [1..7]
    where
        f n = if mod n 2 == 0 then [n*n] else []
-- but... both of these give: `[[],[4],[],[16],[],[36],[]]`
-- which is not what we want... i.e. `[4,16,36]`
-- solution: `concat`
wrapper = concat (map f [1..7])
    where
        f n = if mod n 2 == 0 then [n*n] else []
wrapper = concat (map f [1..8])
    where
        f n = [n*n | mod n 2 == 0]  -- return `[n*n]` if `n` is even, `[]`
-- note: where can be only used with funtion definitions and not in general with expresions, if you wish to execute it with in ghci
-- hence the logic is wrapper in the `wrapper` function

-- the sieve of eratosthenes, an infinite list of prime numbers
-- logic: we know that 2 is the 1st prime no.
-- if we start an infinite list with 2,
-- after elminating 2's multiples from that infinit list,
-- if we look at the new list, then we will get the element as the 2nd prime no... i.e. 3
-- if we eleminate all multiples of 3 then the 1st element in that new list would be the 1st prime no. adter 3... ani this goes on...
primes = seive [2..]    -- note that we are starting with the first prime
    where
        seive (x:xs) = x:(seive [y | y <- xs, mod y x > 0])
-- we can use `[2..]` because of haskell's lazy evaluation, so that at any iteration our funtion won't be stuck at computing the whole list

-- try it out with: 1st 100 prime numbers
take 100 primes

-- expansion or rewriting of `primes`
-- primes
-- seive [2..]
-- 2:(seive [y | y <- [3..], mod y 2 > 0])
-- 2:(seive 3:([y | y <- [4..], mod y 2 > 0]))
-- 2:(3:(seive [z | z <- (seive [y | y <- [4..], mod y 2 > 0]) | mod z 3 > 0]))
-- ... and so on... why is `mod a b > 0` and not `/=`

-- this is NOT a very efficient way of computing primes... in fact it's not efficient at all
-- but its an interesting demonstration of lazy evaluation (without LE it wouldn't have been possible to work with infinite lists)


-- folding through a list

-- example: sum of integers with in a list
sumlist :: [Int] -> Int
sumlist [] = 0
sumlist (x:xs) = x + sumlist xs

-- example: product of integers with in a list
mullist :: [Int] -> Int
mullist [] = 1
mullist (x:xs) = x * mullist xs

-- you might have notices a common pattern
combine :: (Int -> Int) -> Int -> [Int] -> Int
combine f v [] = v
combine f v (x:xs) = f x (combine f v xs)
-- pause and visualize how `(combine f v xs)` will expand
-- `f` is the funtion or operation we want to apply on the list elements
-- `v` is the initial value

-- therefore
sumlist2 l = combine (+) 0 l
mullist2 l = combine (*) 1 l

-- haskell has a builtin funtion called `foldr` that does exactly this i.e. what the funtion `combine` did... i.e. "folding"
-- "r" stands for right.. but what "right"?
-- remember lists in haskell are right associative? i.e. `[1,2,3]` equals `1:(2:(3:[]))`
-- have a look at the following expansion: (if we consider the combine definition)
-- foldr (+) 0 [1,2,3]
-- (+) 1 (foldr (+) 0 [2,3])
-- (+) 1 ((+) 2 (foldr (+) 0 [3]))
-- (+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))
-- (+) 1 ((+) 2 ((+) 3 0))
-- we can see that this is right-associative thingy... hence... "r"
-- this is what means by "folding" a funtion `f` from the right, through the list,  with an initial value `v`

-- therefore
sumlist3 l = foldr (+) 0 l
mullist3 l = foldr (*) 1 l

-- example: length of a list
mylen :: [a] -> Int
mylen l = foldr f 0 l
    where
        f :: a -> Int -> Int
        f _ x = x + 1
-- breakdown
-- mylen "rajat"
-- foldr f 0 "rajat"
-- f 'r' (foldr f 0 "ajat")
-- f 'r' (f 'a' (foldr f 0 "jat"))
-- f 'r' (f 'a' (f 'j' (foldr f 0 "at")))
-- f 'r' (f 'a' (f 'j' (f 'a' (foldr f 0 "t"))))
-- f 'r' (f 'a' (f 'j' (f 'a' (f 't' (foldr f 0 "")))))
-- f 'r' (f 'a' (f 'j' (f 'a' (f 't' 0))))
-- since we know that `foldr` is "right-associativish", that's exactly why we provided a definition of `f` like `f _ x = x + 1`, and hence keeping the definition in my we can simplify the expression as
-- f _ (f _ (f _ (f _ (f _ 0))))
-- f _ (f _ (f _ (f _ 1)))
-- f _ (f _ (f _ 2))
-- f _ (f _ 3)
-- f _ 4
-- 5
-- # classic recursion!!!

-- remember haskell works by outermost reduction?
-- it will rewrite the outermost definition
mylen2 = foldr f 0  -- you can think of this as a partial funtion
-- so, when we write:
mylen2 [1,2,3]
-- the outermost definition i.e. the definition of `mylen2` will be expanded first, like so: `foldr f 0 [1,2,3]`

-- as a RULE you can remember the working of `foldr` funtion as:
-- `foldr f v [x0,x1,...,xn]` equals `f x0 (f x1 (...(f xn v)))`

-- examples:
-- append element x to the right of a list
appendright :: a -> [a]
appendright x l = l ++ [x]

-- what will the operation of the following?
wrapper = foldr appendright []
-- `reverse`
-- it will reverse the list, work it out!
wrapper "rajat"
-- output: "tajar"

-- what will the operation of the following?
wrapper = foldr (++) []
-- `concat`
-- it will "collapse" a multidimentional array into 1D
-- dissolve all inner lists with in a list
-- flattens a list of lists into a single list
wrapper [[1,2,3],[4,5,6],[7,8,9]]
-- output: `[1,2,3,4,5,6,7,8,9]`


-- revisiting the definition of `foldr`:
-- what is its type?
myfoldr :: (a -> b -> b) -> b -> [a] -> b
myfoldr f v [] = v
myfoldr f v (x:xs) = f x (foldr2 f v xs)

-- sometimes there's no natural value to assign to the empty list
-- built-in function `foldr1`
myfoldr1 :: (a -> b -> b) -> [a] -> a   -- is this corect?
myfoldr1 f [x] = x
myfoldr1 f (x:xs) = f x (myfoldr1 f xs)

-- eg
-- myfoldr1 (+) [1,2,3,4]
-- (+) 1 (myfoldr1 (+) [2,3,4])
-- (+) 1 ((+) 2 (myfoldr1 (+) [3,4]))
-- (+) 1 ((+) 2 ((+) 3 (myfoldr1 (+) [4])))
-- (+) 1 ((+) 2 ((+) 3 4))
-- in short myfoldr1 `f [x0,x1,...xn-1,xn]` equals `f x0 (f x1 (f ... xn-2 (f xn-1 xn)))`

-- example: finding the maximum number in a list, it's undefined for an empty list
maxlist = foldr1 max    -- `max` is a built un funtions that gives maximum of two numbers
maxlist [9,0,-1,3,1,99]

-- folding from the left: `foldl`
myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl _ v [] = v
myfoldl f v (x:xs) = foldl f (f v x) xs
-- `foldl f v [x0,x1,...,xn]` equals `f (f (f (f v x0) x1)...) xn`
-- work out:    -- gives correct output, but is the folow correct?
-- foldl (++) [] [[1],[2],[3]]
-- foldl (++) ((++) [] [1]) [[2],[3]]
-- foldl (++) [1] [[2],[3]]
-- foldl (++) ((++) [1] [2]) [[3]]
-- foldl (++) [1,2] [[3]]
-- foldl (++) ((++) [1,2] [3]) []
-- foldl (++) [1,2,3] []    -- base case
-- [1,2,3]

-- example: string to integer
-- the normal approach
char2int :: Char -> Int
char2int c
    | c >= '0' && c <= '9' = ord c - ord '0'
-- notice that we havn't provide an otherwise clause
-- hence if a non-digit characeter is given then an exception will be thrown

str2int :: String -> Int
str2int str = f str ((length str) - 1)
    where
        n :: String -> Int -> Int
        f "" 0 = 0
        f (x:xs) n = (char2int x) * 10^n + (f xs (n-1))

-- we havn't provided a base case like `str2int "" = X` since empty strings cannot converted to integers

-- the "folding" approach
str2int2 :: String -> Int   -- remember to exclude the output if you are defining it like this
str2int2 = foldl nextdigit 0
    where
        nextdigit :: Int -> Char -> Int
        nextdigit i c = 10*i + char2int c
-- expansion
-- str2int "123"
-- foldl f 0 "123"
-- foldl f (nextdigit 0 '1') "23"
-- foldl f 1 "23"
-- foldl f (nextdigit 1 '2') "3"
-- foldl f 12 "3"
-- foldl f (nextdigit 12 '3') ""
-- foldl f 123 ""
-- 123

-- woow! wrapping mind around "folds" is hard...!

-- man cumulative opeations on lists can be expressed in terms of folding a function through the list; foldr foldr1 foldl are then builtin funtions


-- more builtin higher order list funtions

-- example: `takeWhile`, a variation of the `take` function
-- recap: `take` function takes first n elements from a list
-- `takeWhile` takes first (and NOT all) "n" elements which satisfy a property
-- note that "n" is an argument to the funtion
-- when the condition fails, the takewhile stops there and returns all elements before that satisfied the property
-- `takeWhile` function takes an argument (a funcion i.e. key, `map`, `sort`, `filter`, etc. rings a bell?) which decides the criteria i.e. condition based on which items should be "taken"
mytakewhile :: (a -> Bool) -> [a] -> [a]
mytakewhile _ [] = []
mytakewhile f (x:xs)
    | f x = x:(mytakewhile f xs)    -- notice how we are using the actual funtion inside of a local definition
    | otherwise = []
-- `takeWhile (<4) [1,2,3,4,3,2,1]` gives `[1,2,3]`
-- `mytakewhile (<4) [1,2,3,4,3,2,1]` gives `[1,2,3]`

-- example: first position in string `s` where character `c` occurs
pos2 :: String -> Char -> Int
pos2 "" _ = 0   -- return the position of the char, otherwise length of string
pos2 (x:xs) c
    | c == x = 0
    | otherwise = 1 + pos2 xs c

-- using `takeWhile`
pos3 :: String -> Char -> Int
pos3 str c = length (takeWhile (/= c) str)

--- symmetric funtion for `takeWhile` is `dropWhile`

-- `map f x` gives `[f x0, f x1, ..., f xn-1]`
-- `zipWith f la lb` gives `[f la0 lb0, f la1 lb1, ..., f lan-1 lbn-1]`
-- where, `n = min (length la) (length lb)`
-- e.g.
zipWith (+) [0, 1, 2, 3] [3, 2, 1, 0] -- gives `[3, 3, 3, 3]`
zipWith (<) [0, 1, 2, 3] [3, 2, 1, 0] -- gives `[True, True, False, False]`
-- type of `zipWith` is `zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`

-- example:: student's marks
marks :: [[Int]] -> [Int]   -- e.g. `[[1,2,3],[4,5,6],[7,8,9]]` and gives `[12,15,18]`
marks [l] = l
marks (x:y:ys) = marks ((zipWith (+) x y):ys)

-- another more smart approach
marks2 :: [[Int]] -> [Int]
marks2 [l] = l
marks2 (x:xs) = zipWith (+) x (marks xs)    -- this works by expansion/rewritting
-- e.g. 
-- marks2 [[1,2,3],[4,5,6],[7,8,9]]
-- zipWith [1,2,3] (marks2 [[4,5,6],[7,8,9]])
-- zipWith [1,2,3] (zipWith [4,5,6] (marks2 [[7,8,9]]))
-- zipWith [1,2,3] (zipWith [4,5,6] [7,8,9])
-- zipWith [1,2,3] [11,13,15]
-- zipWith [12,15,18]

-- if you have notices this is funtion folding
marks3 :: [[Int]] -> [Int]
marks3 = foldr1 (zipWith (+))

-- simple for of `zipWith`: `zip`
-- combines two list into a list of pairs
zip [1,2,3,4] [1,2] -- give [(1,1),(2,2)]

-- example: check that a list of integers is non-decreasing
nonde :: [Int] -> Bool
nonde [] = True
nonde [x] = True
nonde (x:y:xs) = x <= y && nonde (y:xs)
-- `nonde [1..10]` gives `True`
-- for `[x0,x1,...,xn-1]` we are checking `x0<=x1 && x1 <= x2 && ... && xn-2<=xn-1`

-- `zip` version
nonde2 :: [Int] -> Bool
nonde2 [] = True
nonde2 [x] = True
nonde2 x = and [a < b | (a,b) <- zip x (tail x)]
-- say `x` is `[1,2,3,4,5,6]`
-- then `tail x` will be `[2,3,4,5,6]`
-- and then `zip x (tail x)` will be `[1<2, 2<3, 3<4, 4<5, 5<6]`, keep in `min la lb`

-- another way could be:
nonde3 :: [Int] -> Bool
nonde3 x = and (zipWith (<=) x (tail x))

-- example: position of c in string s
pos4 :: String -> Char -> Int
pos4 s c = head ((allpos s c) ++ [length s])

allpos :: String -> Char -> [Int]
allpos s c = [i | (i, ci) <- zip [0..(length s)] s, c == ci]
-- only `` can be used to find the indices where character `c` occurs
-- note that `zip [0..(length s)] s` and `zip [0..((length s)-1)] s` will rpoduce the same output, since `min la lb`

-- example: initail segments: returns a list of initial segments of a list
-- e.g. `f [1,2,3,4]` gives `[[], [1], [1,2], [1,2,3], [1,2,3,4]]`
iniseg :: [a] -> [[a]]
iniseg [] = [[]]
iniseg (x:xs) = []:map (x:) (iniseg xs)
-- iniseg [1,2,3]
-- []:(map (1:) (iniseg [2,3]))
-- []:(map (1:) ([]:(map (2:) (iniseg [3])))
-- []:(map (1:) ([]:(map (2:) ([]:(map (3:) (iniseg []))))))
-- []:(map (1:) ([]:(map (2:) ([]:(map (3:) [[]])))))
-- []:(map (1:) ([]:(map (2:) ([]:[[3]]))))
-- []:(map (1:) ([]:(map (2:) [[],[3]])))
-- []:(map (1:) ([]:[[2],[2,3]]))
-- []:(map (1:) [[],[2],[2,3]]])
-- []:[[1],[1,2],[1,2,3]]]
-- [[],[1],[1,2],[1,2,3]]]

-- example: interleave: insert x into list l at all possible postions
il :: a -> [a] -> [[a]]
il x [] = [[x]]
il x (y:ys) = (x:y:ys):map (y:) (il x ys)
-- il 1 [2,3]
-- [1,2,3]:map (2:) (il 1 [3])
-- [1,2,3]:map (2:) ([1,3]:map (3:) (il 1 []))
-- [1,2,3]:map (2:) ([1,3]:map (3:) [[1]])
-- [1,2,3]:map (2:) ([1,3]:[[3,1]])
-- [1,2,3]:map (2:) ([[1,3],[3,1]])
-- [1,2,3]:[[2,1,3],[2,3,1]]
-- [[1,2,3],[2,1,3],[2,3,1]]

-- example: permutations
per :: [a] -> [[a]]
per [x] -> [[x]]
per (x:xs) = concat (map (interleave x) per xs)
-- builtin funtion in haskell
-- concatMap f l = concat (map f l)

-- example: patitions, compute all paritions of a list
-- part [1,2,3] = [[[1],[2],[3]], [[1,2],[3]], [[1],[2,3]], [[1,2,3]]]
part :: [a] -> [[[a]]]
part [x] = [[x]]
part (x:xs) = [(x:head l):(tail l) | l <- part xs] ++ [[x]:l | l <- part xs]


-- measuring efficiency
-- how much resources are being used: time and space
-- computer the amount of time required to process a task

-- notion of computing in haskel is rewritting or reduction
-- we rake a function definition and keep replacing its lhs by rhs, and keep doing it until it no longer can be simplified
-- hence it makes sense to count the number of reduction steps and use this as measure of running time of a haskell program

-- the running time normally depends upon the size of the input, it takes more TIME to sort a large list than a small list
-- hence running time is typically expressed as a funtion of input size
-- let T(n) be a funtion that describes the dependence of time on the input size of "n"

-- example: cimplexity of `++`
[] ++ y = y                 -- RULE 1
(x:xs) ++ y = x:(xs++y)     -- RULE 2

-- [1,2,3] ++ [4,5,6]
-- 1:([2,3]++[4,5,6])
-- 1:(2:([3]++[4,5,6]))
-- 1:(2:(3:([]++[4,5,6])))
-- 1:(2:(3:([4,5,6])))
-- we stop here, since the expression can no longer be simplified or reduced further by rewritting or expansion
-- this expresion can be directly evaluated by haskell

-- l1 ++ l2:
-- it is clear that for each element in the `l1` we've to apply RULE 2 once
-- i.e. RULE 2 is used `length l1` times
-- and when `length l1` becomes 0, RULE 2 is applied once
-- this behaviour is independent of the values within `l1` nd `l2`

-- example: element of
elem :: Int -> [Int] -> Bool
elem _ [] = False
elem n (x:xs)
    | x == n = True
    | otherwise = elem n xs

-- elem 3 [4,7,8,9]
-- elem 3 [7,8,9]
-- elem 3 [8,9]
-- elem 3 [9]
-- elem 3 []
-- False

-- elem 3 [3,4,7,8]
-- True

-- in this case, time complexity depends on input size "AND" value
-- in the previous example, it was only dependent on the lenght if 1st input

-- for accounting variations accross input values, the standard idea is to look for worst possible input
-- i.e. the input that takes maximum running time
-- this is called as worst case time complexity
-- however this is a bit pessimistic... as this might be a rare condition, since not every input will be "worst"
-- a more realistic/concrete metric would be finding out the average case... but it is difficult and/or impossible to compute, so unfortunately we have to settle for worst case complexity


-- another feature that is usually used when analysing algorithms is asymptotic complexity
-- in this, we are interested in how T(n) grows as a funtion of "n", but we are only interested in orders of magnitude rather than the exact details of the constants involved
-- standard way of usage: the big O notation
-- big O notation says:
-- f(n) is no bigger than g(n)
-- in other words, f(n) is dominated by some constant times g(n) for every n>0
-- e.g. f(n) = an^2 + bn + c, a quadratic funtion
-- we claim that this is O(n^2)
-- e.g. 3n^2 + 5n + 2, we can take k = a+b+c, if a,b,c > 0
-- we could say that <=10n^2, n>0... for big O notation
-- we usually ignore the constant factors and lower order terms
-- and take the highest order term
-- O(n), O(n log n), O(n^k) (for polynomial funtions), O(2^n) (for exponential funtions), etc.

-- therefor complexity of:
--  1. ++ : O(n), where n is the length of first list, this does not depend upon the length of the 2nd list, nor the values of 1st/2nd list
--  2. elem : O(n), this is worst case, as it may terminate in one step

-- example: reverse a list
myrev :: [a] -> [a]
myrev [] = []   -- RULE 1
myrev (x:xs) = (myrev xs) ++ [x]    -- RULE 2

-- to analyse this funtion, we could do it directly like `++` and `elem`
-- or write a recurrance for T(n) (i.e. a funtion that describes the dependence of time on the input size of "n")

-- for input (list) size of 0, funtion `myrev` will run once... according to RULE 1, i.e. T(0) = 1
-- for non-zero "n", recurrance of T(n) will be:
-- T(n) = T(n-1) + n
-- for reversing a list of length n, we have to reverse a list of length n-1 (i.e. `(myrev xs)` part of the expression `(myrev xs) ++ [x]`)... according to RULE 2
-- and ++ funtion takes time propotional to the length of 1st list, hence it will take another n steps, therefore n in the expresion "T(n) + n"

-- the easiest way to solve recurance is solve it by expanding:

-- T(n) = T(n-1) + n
-- T(n) = (T(n-2) + n-1) + n
-- T(n) = (T(n-3) + n+2) + n-1 + n
-- ...
-- T(0) + 1 + 2 + ... + n
-- 1 + 1 + 2 + ... + n
-- 1 + n(n+1)/2
-- O(n^2)   -- here we have ignore the constant 1 in the expr "1 + n(n+1)/2 "

-- we are spending n^2 time for reversing a list of n elements, inefficient!
-- can we do better? yes!
-- idea: consider you have a stack of books, you remove the 1st book keep it down, remove the 2nd book and keep it above the 1st... and so on until you are done with the stack... you will find out that the new stack is the reverse of the original one!

fastrev :: [a] -> [a]
fastrev l = transfer l []

transfer :: [a] -> [a] -> [a]
transfer [] l = l
transfer (x:xs) l = transfer xs (x:l)

-- T(0) = 1
-- T(n) = T(n-1) + 1
-- ...
-- 1 + 1 + ... + 1
-- O(n)

-- you need to understand the laguage specific computational model in order to come up with an efficient solution
-- you just cannot blindly apply concepts of one programming language to another


-- sorting
-- insertion sort:
-- you take an element (e.g. a card or a number) place it at a initial postion
-- take 2nd and place it before or after the 1st element
-- now insert the 3rd element in appropriate place
-- and so on...

-- example: insert an element into a sorted list
ins :: Int -> [Int] -> [Int]
ins n [] = [n]
ins n (x:xs)
    | n <= x = n:x:xs
    | otherwise = x:(ins n xs)
-- worst case complexity T(n) = O(n)

isort :: [Int] -> [Int]
isort [] = []   -- RULE 1
isort (x:xs) = ins x (isort xs)  -- RULE 2

-- isort [2, -1, 4 ,0, 5]
-- insert 2 (isort [-1, 4 ,0, 5])
-- insert 2 (ins (-1) (isort [4 ,0, 5]))
-- insert 2 (ins (-1) (ins 4 (isort [0, 5])))
-- insert 2 (ins (-1) (ins 4 (ins 0 (isort [5]))))
-- insert 2 (ins (-1) (ins 4 (ins 0 (ins 5 isort []))))
-- insert 2 (ins (-1) (ins 4 (ins 0 (ins 5 []))))
-- insert 2 (ins (-1) (ins 4 (ins 0 [5])))
-- insert 2 (ins (-1) (ins 4 [0, 5]))
-- insert 2 (ins (-1) [0, 4, 5])
-- insert 2 [-1, 0, 4, 5]
-- [-1, 0, 2, 4, 5]

-- recap: `foldr f v [x0,x1,...,xn]` equals `f x0 (f x1 (...(f xn v)))`
-- e.g. `foldr (+) 0 [1,2,3]` = `(+) 1 ((+) 2 ((+) 3 0))`

isort2 = foldr ins []

-- recurrence
-- T(0) = 1     -- ... RULE 1
-- T(n) = T(n-1) + O(n)
-- T(n-1) because according to rule 2, `ins x (isort xs)` takes T(n-1) time
-- O(n) because according to rule 2, `(isort xs)` takes O(n) time
-- therefore, T(n) = O(n^2)

-- a better statergy would be to divide and conquer
-- we divide the list into two halfs and sort them
-- then merge those sorted lists together
-- merging operation is easy, say you have two sorted lists of numbers as stacks
-- then you look at both stacks from the top and keep the smallest of the top element on the floor
-- you keep repeating the above step until the stacks are exhausted
-- note that in this case: stack 1 = [1,2,3], stack 2 = [4,5], you simply cannot put 1 and then 4, because 2 is still less than 4, so you have to compare the tops after keeping an element on the floor

-- so summarizing it, say we have `l` as the list we want to sort
-- so we divide it into two halfs as:
--  1. `l!!0` to `l!!(n/2 - 1)`
--  2. `l!!(n/2)` to `l!!(n - 1)`
-- then we sort the two halfs and then merge them back together
-- note that for sorting the two halfs we again emply the same statergy
-- i.e. divide, sort, merge...

-- congarts!! you have just discovered "merge sort"!

-- e.g.
-- [43 32 22 78 63 57 91 13]
-- [43 32 22 78] [63 57 91 13]
-- [43 32] [22 78] [63 57] [91 13]
-- [43] [32] [22] [78] [63] [57] [91] [13]  -- a list of length 1 is by default a sorted list
-- [32 43] [22 78] [57 63] [13 91]  -- now from here on, we apply merge sort
-- [22 32 43 78] [13 57 63 91]
-- [13 22 32 43 57 63 78 91]

-- merge two (implicitly assumed) sorted list
merge :: [Int] -> [Int] -> [Int]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys)
    | x <= y = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)
-- analysis: each comparison adds one element to the output
-- so... T(n) = O(n), where n is the sum of lenths of input lists

-- merge sort
msort :: [Int] -> [Int] -> [Int]
msort [] = []
msort [x] = [x] -- absense of this will give us infinite loop
mergesort l = merge (mergesort (front l)) (mergesort (front l))
    where
        front :: [Int] -> [Int]
        front l = take ((length l) `div` 2) l
        back :: [Int] -> [Int]
        back l = drop ((length l) `div` 2) l

-- msort analysis: O(n log n)
-- don't ask me how... somewhere between 15 to 16 of PV8_scc1s3g


-- another improvement we can make in this type of sorting is:
-- when we sort the original list in to two halves, we make sure that lower half, after sorted can direcly we stick it with the sorted upper halh, in order to avoide merging as an imprvement
-- but for that we will need to find the middle value of the original list, and for that we actually would have to sort the list which is out aim!
-- so, we pick a pivot (typically the 1st element of the original list) and make two halfs from it
-- lower half contains all elements lower that the pivot value and upper contains elements greter than the pivot value
-- next we would sort the two parts, and then stick all of them as:
-- <sorted_lower_part><pivot><sorted_upper_part>
-- we would recursively be sorting any further half partitions
-- this is called as "quicksort"
-- e.g.
-- [43 32 22 78 63 57 91 13]    -- pick 43 as the pivot
-- [32 22 13] [43] [78 63 57 91]    -- rearranged such that x<=43 = lower half, x>43 = upper half
-- now we can recursively sort the upper and lower halfs


qsort :: [Int] -> [Int]
qsort [] = []
-- qsort (x:xs) = (qsort [y | y <- xs, y <= x]) ++ [x] ++ (qsort [z | z <- xs, z > x])
-- following is more readable
qsort (x:xs) = (qsort lower) ++ [pivot] ++ [qsort upper]
    where
        pivot = x
        lower = [y | y <- xs, y <= x]]
        upper = [z | z <- xs, z > x]
-- we exclude the splitter or pivot value from lower or it might result into duplication

-- qsort analysis:
-- if pivot is min or max value of the list, then either of the halfs would be empty, and other would be of size n-1
-- therefore: worst case, as we don't or can't have control over pivot value
-- T(n) = T(n-1) + (n-1) + n = 1 + 2 + ... + n = O(n^2)
-- already sorted array is worst case input
-- so, quicksort hasn't acieved anything since thw worst case time complexity equals to the naive insertion sort
-- BUT...
-- the average case for quick sort is computable and is O(n log n)
-- between 21 and summary, what it means to compute average case is give, have alook if u r interested!


-- using infinite lists
-- lazy evaluation is technically called as outermost reduction
-- consider `f e`, where `f` is a funtion and `e` is an argument
-- so haskell, will simplify the funtion definition first, and comptue the argument ONLY IF it's needed
-- this allows us to sensibly use infinite lists like: `head [1..]`

-- example: consider a directed graph
-- edge 'X' 'Y' = True  -- there's a path/edge from `X` to `Y`, but not vice-versa
edge :: Char -> Char -> Bool
edge 'A' 'B' = True
edge 'A' 'D' = True
edge 'B' 'C' = True
edge 'C' 'A' = True
edge 'C' 'E' = True
edge 'D' 'E' = True
edge 'F' 'D' = True
edge 'F' 'E' = True
edge _ _ = False

-- now we want to check connectivity between two vertices
-- `connected x y` is True only if there's a path from x to y using the given set of edges
-- inductive definition: we can find an intermediate vertice z between x and y
-- unfortunately this is not an easy task in a funtion programming language
-- alternative to achieve the same: building paths of longer and longer lengths
-- extend a path of length k to length k to k+1 by adding an edge, where length is the number of vertices in a path

type Path = [Char]
extendpath :: Path -> [Path]
extendpath [] = [[c] | c <- ['A'..'F']]
extendpath p = [p++[c] | c <- ['A'..'F'], edge (last p) c]
-- this funtion takes in a path, looks at its last vertice and then return a list of Paths those last element has a link one step further

extendall :: [Path] -> [Path]
extendall [] = [[c] | c <- ['A'..'F']]
extendall l = concat [extendpath p | p <- l]
-- extendall l = [ll | p <- l, ll <- extendpath p]

-- built-in funtion in haskell: iterate
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x = [x, f x, f (f x), f (f (f x)), ...]   -- infinite lists

-- to genrate all paths
allpaths = iterate extendall [[]]
-- this will generate a list of "list of paths" of all lengths: 0, 1, 2, 3, ...

-- to check if x and y are connected, we need to check for paths without loops from x to y, e.g. consider a -> b -> d -> b -> d -> c, here we must ensure that there are no loops like: b -> d -> b

-- given n nodes overall, a loop free path can have at most n-1 edges
-- suffices to examine first n+1 entries in allpaths
-- take (n+1) allpaths
-- note that path of length 0 and 1 are note really paths... so
-- drop 2 (take (n+1) allpaths)

-- extract the endpoints of paths of length at most n
connectedpairs = [(head p, last p) | l <- firstn, p <- l]
    where
        n = length ['A'..'F']
        allpaths = iterate extendall [[]]
        firstn = drop 2 (take (n+1) allpaths)

connected x y = elem (x,y) connectedpairs

-- extend all generates path with with loops, but all we care about is are two vertices connected? and not the shorted path between them

-- backtracing: the N Queens problem
-- place N queens on NxN chessboard so that no two queens attack each other

-- place the first queen somewhere in the first row
-- in each succeeding row, place a queen at the leftmost square that is not attacked by any of the earlier queens
-- after placing some queens less than N, if there's no place for the next one then you "backtrack" and try different position and continue
-- repeat till you hit the goal

-- use infinite lists without explicitly worrying about backtracking
-- काही डॊक्यात नाही घुसत आहॆ... to be continued... 23 to the end pjWSqgoucEg



-- conditional polymorphism
-- a type class is a collection of types with a required property
-- type class `Ord` contains all types whose values can be compared
-- `Ord t` is a "predicate" that evaluates to true if a type `t` belongs to type class `Ord`
-- if `Ord t` (i.e. if the condition eevaluates to true), then <, <=, >, >=, /= are defined for `t`
-- this is a conditional polymorphism
-- the ability of something to have or to be displayed in more than one form
-- the provision of a single interface to entities of different types or the use of a single symbol to represent multiple different types

-- so now we can write:
quicksort :: (Ord a) => [a] -> [a]
-- this should be read as: if `a` belongs to `Ord`, then `quicksort` is of type `[a] -> [a]`
-- `=>` you should read this as a logical implication
-- in simpler words the type `[a]` must be a list of comparable items

-- example: consider a list of higher order types namely funtions
funclist = [(^2), (+3), (*5)] :: [Int -> Int]   -- note that this is a valid syntax
-- where `Int -> Int` is the type of each funcion in the list
-- how to evaluate whether a funtion belongs to `funclist`?
-- i.e. elem f funclist
-- can we check `f == g` for funtions

-- `Eq a` holds if ==, /= are defined on a
elem :: (Eq a) => a -> [a] -> Bool

-- remember funtion sum that sums all elemets in a list?
-- we require all elements of that list to support arithmetic operations on
-- `Num a` evaluates to tru if `a` supports basic arithmetic operations like `+`
-- so the correct polymorphic type for `sum` is:
sum :: (Num a) => [a] -> a
-- take a look at specialized subclasses like `Integral`, `Frac` in haskell
-- important called `Show`, it tells when a value can be displayed to the user

-- in genral, a type class is defined by a signature i.e. funtions that the types in the class must support
-- any funtion, any type can be made a member of a type class by providing suitable funtion definitions for specic operations
-- e.g. for funtion/type `abc`, to be a member of `Eq` type class == and /= must be defined for that funtion/type


-- defining funtions in ghci
-- in normal haskell code, `let` is similar to `when`

dist (x1,y1) (x2,y2) = sqrt (diffx^2 + diffy^2)
    where
        diffx = x2-x1
        diffy = y2-y1

dist (x1,y1) (x2,y2) =
    let diffx = x2-x1
        diffy = y2-y1
    in sqrt (diffx^2 + diffy^2)
-- these are two seemingly two equivalent ways

-- `let ... in ...` is a haskell expression, like `if ... then ... else ...`
-- can be used whenever an expression is allowed

-- at INTRODUCTORy level the difference between them is minor, BUT THEY ARE NOT EQUIVALENT!!!

-- in ghci you can use `:{` and `:}` for multiline inputs
-- semicolons can also be used for seperating out lines
-- if you don't do either, the latest will override any previous definitions
-- use `let` before stating the definition
-- in GHCi versions (8.0+), let is not required


-- user-defined datatypes
-- `data` keyword is used to define new types
-- data Bool = False | True
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat

-- data types with parameters
data Shape
    = Circle Float
    | Square Float
    | Rectangle Float Float
-- `Circle 5.0`, `Rectangle 3.0 4.0` are some examples

-- funtions that can be defined using pattern matching
weekend :: Day -> Bool
weekend Sat = True
weekend Sun = True
weekend _ = False

area :: Shape -> Float
area (Circle r) = pi*r*r
area (Square a) = a*a
area (Rectangle l b) = l*b
    where
        pi = 3.14159

-- other ways of defining funtions on user defined data types
weekend2 :: Day -> Bool
weekend2 d
    | d == Sun || d == Sat = True
    | otherwise = False
-- defining this results into an error: No instance for (Eq Day) arising from a use of ‘==’

-- to check equality if two values with data type `a`, `a` must belong to the type class `Eq
-- add `Day` to the type class `Eq` as follows:
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving Eq
-- default behaviour: `Sun == Sun`, `Tue /= Fri`
-- by this `weekday2` should compile fine

nextday :: Day -> Day
nextday Sun = Mon
nextday Mon = Tue
nextday Tue = Wed
nextday Wed = Thu
nextday Thu = Fri
nextday Fri = Sat
nextday Sat = Sun
-- executing `nextday Mon` will lead to error: No instance for (Show Day) arising from a use of ‘print’

-- to make `nextday` work, we must make `Day` an instance of Show
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving (Eq, Show)
-- the type class `Show` consists of all data types that implement the funtion `show`
-- `show` converts its inputs to a string which can be printed on the screen
-- default representation is the text representaiton itself
-- e.g. the datatype `Wed` would be display as `"Web"`

-- `Ord` means ordinal
data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving (Eq, Show, Ord)
-- this would mean that: Sun < Mon < Tue < Wed < Thu < Fri < Sat
-- the order is determined by the order in which the vales are enumerated


data Shape
    = Circle Float
    | Square Float
    | Rectangle Float Float
    deriving (Eq, Show, Ord)
-- this would mean:
-- `Square 4.0 == Square 4.0` will evaluate to true due to `Eq`
-- `Circle 5.0 /= Rectangle 3.0 4.0` will evaluate to True
-- `Circle 5.0` will have test representation as "Circle 5.0" sue to `Show`
-- `Square 4.0 > Circle 5.0` will evaluate to true due to `Ord`

-- here `Square`, `Circle`, `Sun`, `Mon`, etc. are constructors
-- they behave like any other funtion
-- Sun :: Day
-- Rectangle :: Float -> Float -> Shape
-- Circle :: Float -> Shape
-- `Sun` constructor take in no inputs but produces an output of type `Day`
-- `Rectangle` funtion take two Float type inputs and produces an output of type `Shape`

map Circle [3.0, 2.0]
-- wil give `[Circle 3.0, Circle 2.0]`

-- in haskel the convention is that if a data type has only one constructor (like the following and unlike `Shape` data type) the same name is use for the constructor as that of the data type
data Person = Person String Int Float String
    deriving Show
-- example:
guy = Person "Rajat" 21 5.8 "+919988776655"

-- extration of name, age, etc of Persion type of object
name :: Person -> String
name (Person n _ _ _) = n
age :: Person -> String
age (Person _ a _ _) = a
height :: Person -> String
height (Person _ _ h _) = h
phone :: Person -> String
phone (Person _ _ _ p) = p
-- this is very combersome... haskell offers an easier alternative syntax

-- you can define `Person` as:
data Person = Person {
    name :: String,
    age :: Int,
    height :: Float,
    phone :: String
} deriving Show
-- these field names are acutlly funtions
name :: Person -> String
age :: Person -> Int
-- so on...

-- now we can define guy as:
guy = Person {name="ABC", age=21, height=4.1, phone="+919988776655"}
-- or alternatilvely as previously as well
-- guy = Person "Rajat" 21 5.8 "+919988776655"

-- summary:
-- `data` keyword is used to declare new data types
-- `deriving` keyword is used to derive a data type as an instace of a type class
-- data types with parameters like: `Shape` and `Person`
-- Sum type or union: `Day` and `Shape`
-- Product type or struct: `Person`


-- abstract data types
-- consider a stack
-- a colletion of Ints stacked on top of the other
-- push: place an element on the top of the stack
-- pop: remove the topmost elemet of the stack

type Stack = [Int]
push :: Int -> Stack -> Stack
push x s = x:s
pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)
insert :: Int -> Int -> Stack -> Stack
insert x n s = (take (n-1) s) ++ [x] ++ (drop (n-1) s)

-- here the internal representation of the Stack is evendent
-- Stack is just oa synonym of [Int]
-- we may not always want this "transpirancy" of the internal representation
-- we would only want push, pop operationson a stack, and strictly nothing else
-- perhaps operations like check if the stack is empty
-- this is the motivation for Abstract data types

data Stack = Stack [Int]
-- the value constructor Stack is a funtion that converts a list

empty :: Stack
empty = Stack []

push :: Int -> Stack -> Stack
push x (Stack xs) = Stack (x:xs)

pop :: Stack -> (Int, Stack)
pop (Stack (x:xs)) = (x, Stack xs)

isempty :: Stack -> Bool
isempty (Stack []) = True
isempty (Stack _) = False

-- previous definitions were Int specific
-- so moving on to polymorphic user-defined data types
data Stack a = Stack [a]
    deriving (Eq, Show, Ord)
empty :: Stack a
push :: Int -> Stack a -> Stack a
pop Stack a -> (a, Stack a)
isempty :: Stack a -> Bool
-- here the types are more general

-- type parameters
sumstack (Stack xs) = sum xs
-- sometimes funtions on these data types may not be completely polymorphic but conditionally polymorphic
sumstack :: (Num a) => Stack a -> a

-- a custom `show`
primtelems :: (Show a) => [a] -> String
printelems [] = ""
printelems [x] = show x
printelems (x:xs) = show x ++ "->" ++ printelems xs

-- here, `instance` is a keyword
instance (Show a) => Show (Stack a)
    where
        show (Stack l) = printelems l

-- let's condiser "Queues" data type
-- a collection of Ints arranged in a sequence
-- enqueue: add an element at the end of the queue
-- dequeue/pop: remove the element at the start of the queue

data Queue a = Queue [a]

empty :: Queue a
empty = Queue []

isempty :: Queue a -> Bool
isempty (Queue []) = True
isempty (Queue _) = False

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue xs) = Queue (xs ++ [x])
-- each enque on a queue of length n takes O(n) time

dequeue :: Queue a -> (a, Queue a)
dequeue (Queue (x:xs) = (x, Queue xs)
-- enqueueing and dequeueing n elements might take O(n^2) time

-- efficient queue: explained in 14 and onwards
-- maintained two halfs of the queue as: [q0,q1,q2,...,qi] [qn,qn-1,...,qi+1]
-- in this approach, at times, we would be requiring to reverse the 2nd list when the 1st gets eventyally... (or something like that! watch the video VERY carefully!)
-- next follows amortized analysis


--- modules
-- a module consits of funtions related to each other
-- name of the file must match with the name of the module
-- module can be used in any other file in the same directory
-- modules can be used to hide implementation details

-- i skipped the "Queue" module, coz that was a bouncer

-- example: stack module
module Stack(Stack(), empty, push, pop, isempty, show) where
-- funtions mentioned here, within the parenthesis are only visible to the outside world, we can define other auxillary funtions as normally we would, but they won't be avaialable outside the module (i.e. aux func. would be hidden from module users)
-- module stack can be imported in another file by using `import Stack`
-- a module facilitates us to hid the internal representation of the funtions/datatypes it sefines from the outside world

data Stack a = Stack [a]
empty = Stack []
push x (Stack xs) = Stack (x:xs)
pop (Stack (x:xs)) = (x, Stack xs)
isempty (Stack []) = True
isempty (Stack _) = False

primtelems :: (Show a) => [a] -> String
printelems [] = ""
printelems [x] = show x
printelems (x:xs) = show x ++ "->" ++ printelems xs
instance (Show a) => Show (Stack a)
    where
        show (Stack l) = printelems l

-- extended example of stacks
-- postfix expression is an arithmetic expr. ehere the operator apprears AFTER the operands
-- no parenthesis required in postfix expr.
-- eg
-- 3 5 8 * + = (3 (5 8 *) +) = 43
-- 2 3 + 7 2 + - = ((2 3 +) (7 2 +) -) = -4


import Stack

-- a post fix expresion would be given as a list of integers and operators
-- a token is either an integer (given by data constructor Val) or an operator (given by const. Op)
data Token = Val Int | Op Char
type Expr = [Token]

evalstep :: Stack Int -> Token -> Stack Int
evalstep st (Val i) = push i st
evalstep st (Op c)
    | c == '+' = push (v2+v1) st2
    | c == '-' = push (v2-v1) st2
    | c == '*' = push (v2*v1) st2
    where
        (v1, st1) = pop st
        (v2, st2) = pop st1

evalexpr :: Stack Int -> Token -> Stack Int
evalexpr st [] = fst (pop st)
eval st (t:ts) = evalExp (evalstep t st) ts

-- a slighty more robust definition
abc :: Int -> Int
abc 0 = error "No zeros!"
abc n = n
-- `error` funtion stops execution and displas a message

-- but what if we don't want out program to abort?
-- or when we don't have any idea about the context our funtion would be used in
-- possible solution: use the built in type constructor Maybe
-- Maybe a = Nothing | Just a
mymax :: [Int] = Maybe Int
mymax [] = Nothing
mymax [x] = Just x
mymax (x:xs)
    | x > y = Just x
    | otherwise = Just y
    where
        Just y = mymax xs

printmax :: [Int] -> String
printmax l = case (max l) of
    Nothing -> show "Empty list"
    Just x -> show "Maximum " ++ show x

-- example: a table fatatype that stores key-value pairs
type key = Int
type Value = String
type Table = [(Key, Value)]

mylookup :: Key -> Table -> Maybe Value
mylookup k [] = Nothing
mylookup k ((k1,v1):kvs)
    | k == k1 = Just v1
    | otherwise = mylookup k kvs
-- this is more robust than returning error or some default value on absense of key

-- builtin funtion
-- lookup :: Eq a => a -> [(a,b)] -> Maybe b


-- skipping
--- recursive data types
--- binary search trees
--- balanced search trees

--- arrays
-- compute nth fibonacci number
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-- this is a straighforward and literal tranlation of mathematical definition of fibonacci series
-- the program is quite simple but...
-- it makes a lots of recursive calls inorder to computer the same value
-- for instance:
-- `fib 3` makes 2 calls to `fib 1`
-- fib 3
-- fib (3-1) + fib (3-2)
-- fib 2 + fib 1                -- here
-- (fib (2-1) + fib (2-2)) + 1
-- (fib 1 + fib 0) + 1          -- and here
-- 1 + 1 + 1
-- 3

-- let G(n) be the funtion that determines the number of calls made to `fib 0` for n > 1
-- for n=2 and n=3, `fib 0` is called 1
-- for n>3, G(n) = G(n-1) + G(n-2), since there's  one call to `fib (n-1)` and one call to `fib (n-2)`
-- by induction hyposthesis G(n-1) = F(n-3) and G(n-2) = F(n-4
-- thus G(n) = F(n-3) + f(n-4) = F(n-2)
-- hence there are exponentially many calls to `fib 0` for `fib n`

-- for an easy fix (often employed in other languages), the computed values are stored in an array rather than recomputing them
-- this approach takes time proportional to "n" rather than "n^2" as in previous case
-- simplifying this even futher would involve keeping track of previos value and current value, rather than using an array

-- this is something that just pooped in my mind
fib2 0 = 1
fib2 1 = 1
fib2 n = compfib 1 1 n
    where
        compfib _ c 1 = c
        compfib p c n = compfib c (p+c) (n-1)


-- linear time fibonacci, laziness to the rescue
fastfib n = fibs!!n
fibs :: [Int]
fibs = 1:1:(zipWith (+) fibs (tail fibs))
-- 1:1:zipWith (+) [1,1,...] [1,...]
-- 1:1:(1+1):zipWith (+) [1,2,...] [2,...]
-- 1:1:2:(1+2):zipWith (+) [2,3,...] [3,...]
-- 1:1:2:3:(2+3):zipWith (+) [3,5,...] [5,...]
-- 1:1:2:3:5:...

-- example: longest common subsequence
-- find the length of lcss of str1 and str2
-- lcss "agcat" "gact" is 3, "gat" is the subsequence
-- lcss "abracadabra" "bacarrat" is 6, "bacara" is the subsequence
lcss "" _ = 0
lcss _ "" = 0
lcss (c:cs) (d:ds)
    | c == d = 1 + lcss cs ds
    | otherwise = max (lcss (c:cs) ds) (lcss cs (d:ds))
-- same problem like fib...
-- lcss cs ds takes time >= 2^n, when cs and ds are of length n
-- solution: store the computed values for efficiency

-- skipping linear sort example

import Data.Array
-- Int is index, Char is the homogeneous type 
myarray :: Array Int Char
myarray = listArray (0,2) ['a','b','c'] -- array (0,2) ['a','b','c']

-- type if listArray
-- listArray :: Ix i => (i,i) -> [e] -> Array i e
-- where, Ix is the type class that contains type which can be used as indices
-- first parameter (i,i) specifies the smallest and largest value of index
-- 2nd argument [e] is the list of values to be stored in array

-- examples
listArray (1,1) [100..199]      -- array (1,1) [(1,100)]
listArray ('m', 'o') [0,2..]    -- array ('m','o') [('m',0),('n',2),('o',4)]
listArray ('b','a') [1..]       -- array ('b','a') []
listArray (1,3) ['a','b']       -- (1,3) [(1,'a'),(2,'b'),(3,*** Exception: (Array.!): undefined array element

-- value is accessed using single exclamaation, unlike doub le for lists
-- error is thrown if you try to access element outside of index bounds

-- associative list
-- array :: Ix i => (i,i) -> [(i,e)] -> Array i e
-- need not to be in ascending order
-- myarray = array (0,2) [(1,"one"),(0,"zero"),(2,"two")]
-- may omit elements 
-- array (0,2) [(0,"abc"),(2,"xyz")]

-- `array` call takes time proportional to the range of indices

-- any type belonging to the type class Ix must provide the funtions
-- range, index, inRange, rangeSize

-- range gives the list of indices in the subrange defined by the bounding pair
-- range :: (a,a) -> [a]
range (1,2) -- [1,2]
range ('m','n') -- "mnop"
range ('z','a') -- ""

-- index =gives the position of a subscript in the subrange
-- index :: (a,a) -> a -> Int
index (-50,60) (-50)    -- 0
index (-50,60)  35  -- 85
index ('m','p') 'o' -- 2
index ('m','p') 'a' -- Exception: Ix{Char}.index: Index ('a') out of range (('m','p'))

-- inRange returns True is a given subscript lies in the range defined bt the bounding pair
-- inRange :: (a,a) -> a -> Bool
inRange (-50,50) (-50)  -- True
inRange (-50,60) 35 -- True
inRange ('m','p') 'o' -- True
inRange ('m','p') 'a' -- False

-- rangeSize gives the size of the subrange defined by the bounding pair
-- rangeSize :: (a,a) -> Int
rangeSize (-50,60)  -- 11
rangeSize ('m','p')  -- 4
rangeSize (50,0)  -- 0

-- funtions on arrays
-- value at the given index
-- (!) :: Ix i => Array i e -> i -> e

-- bounds :: Ix i => Array i e -> (i,i)
-- the bounds with which an array was constructed

-- indices :: Ix i => Array i e -> [i]
-- the list of indices of an array in ascending order

-- elems :: Ix i => Array i e -> [e]
-- the list of elements of an array in index order

-- assocs :: Ix i => Array i e -> [(i,e)]
-- list of associated of an array in index order

-- example fiboonacci using arrays
fib :: Int -> Integer
fib n = fibA!n

fibA :: Array Int Integer
fibA = listArray (0,n) [f i | i <- [0..n]]
    where
        f 0 = 1
        f 1 = 1
        f i = fibA!(i-1) + fibA(i-2)
-- fibA array is used is used enven before it is completely ddefines, thanks to haskell's laziness
-- works in O(n) time

lcss :: String -> String -> Int
lcss str1 str2 = lcss' 0 0
    where
        m = length str1 - 1
        n = length str2 - 1
        lcss' i j
            | i > m || j > n = 0
            | str1!!i == str2!!j = 1+ lcss' (i+1) (j+1)
            | otherwise = max (lcss' i (j+1) (lcss (i+1) j)

-- skipping lcss using arrays
-- skipping accumArray and linear time sort using accumarray


--- input output
-- accepting user inputs
-- print output and diagnostics on screen or to a file

-- execution of a haskell program (which is a bunch of funtions) start with `main` funtion
-- every standalone haskell program should have a `main` funtions

-- the legendary hello world
main = putStr "Hello, world!\n"
-- simplest compilable haskell program
-- save this into hello_world.hs file
-- and compile it using `ghc hello_world.hs` (notice the absence of "i" in ghc)
-- ghc is glassgow haskell compiler ("i" stands for interactive or interpreter)
-- the command generates: hello_world.hi, hello_world.o, hello_world
-- hello_world is the file of interest, it has execute funtions
-- -rwxr-xr-x 1 rajat rajat 3.9M Sep 19 09:46 hello_world
-- -rw-r--r-- 1 rajat rajat  670 Sep 19 09:46 hello_world.hi
-- -rw-r--r-- 1 rajat rajat 3.1K Sep 19 09:46 hello_world.o
-- run executable with `./hello_world`
-- ghci --help and ghci --show-options gives a lots of more info

-- `putStr str` prints the `str` on the screen
-- putStr is of type `String -> b`, for some type b
-- but the return value is not used at all, so perhaps it returns nothing of significance
-- the type (), like an empty tuple, which consists of a single value, can be used to model nothing
-- so is putStr of tpye `String -> ()`?
-- but it does not return the value and how do we account for the side effect of printing something on screen?
-- let's check the type of `putStr` in ghci
-- ghci> :t putStr
-- putStr :: String -> IO ()
-- ghci> :t putStr "hello world"
-- putStr "hello world" :: IO ()

-- what is a IO?
-- IO is a type constructor
-- the value constructor and internal structure of List, Btree, etc. are visible
-- however this is not the case with IO, so we cannot do any pattern matching

-- we can understand io as:
-- data IO a = IO (RealWorld -> (RealWorld,a))
-- IO on lhs is a "type" constructor, and IO on rhs is a "value" constructor
-- "RealWorld" is not a type in haskell, think of it as just a way of understanding what IO means
-- assume that there's a type that represents all states of the RealWorld
-- we can assume IO as taking the "parent state of the real world" and "producing the new state of the real world" (due to the "side effects") and also producing the value of type `a`
-- in other words, objects of `IO a` constitute both a value of type `a` and a "side effect" (i.e. the change in state of the world)

-- technically an object of type `IO a` is not a funtion but an IO action
-- an IO action produces an side effect where a "value" is extracted
-- any funtion that produces a "side effect" will have a type `IO a`

-- comming back to putStr
-- putstr :: String -> IO ()
-- putStr take a tring as an argument and returnes (), producing a side effect when the return value is extracted
-- the side effect is that of printing on screen the string provided as argument
-- `main` is always of typ `IO a` for some `a`

-- side effects in course of executing an action can be:
-- printing on screen
-- reading user input from terminal
-- opening or closing a file / writting into a file
-- changing a directory

-- `putStrLn str` is equivalent to `putStr (str ++ ['\n'])

-- standalone actions are not of much use unless we can perfom a lot of actions
-- haskell provides a way to chain aactions
-- we use the command do to chain multiple actions
-- example:
main = do
    putStrLn "hello!"               -- action 1
    putStrLn "What is you name?"    -- action 2
-- do makes the actions take effect in sequential order, one after another
-- indentation is important, you can also use braces like so:
-- do { ...actions_sepeated_by_semicolons }
-- actions can occur inside let, where, etc. e.g.
main = do { act1; act2 }
    where
        act1 = putStr "Hello, "
        act2 = putStrLn "world!"

-- more actions
print :: Show a => a -> IO ()
-- output a value of any printable type to the standard output (screen), and adds a new line, (putStr only prints string)

putChar :: Char -> IO ()
-- writes the Char argument to the screen

getLine :: IO String
-- read a line from the standard input and return it as a string
-- the side effect of getLine is the consumption of a line of input, and the return calue is a string
-- so other cases like putStr putChar, it makes sense that the value extracted is nothing i.e. (), but in this case of getLine the value extracted (of return value `a`) is String

getChar :: IO Char
-- read the next character from the standard input

-- binding
-- getLine is of type `IO String`
-- how to use its return value?
-- we need to "bind" the return value to an object of type `String` and then use it
-- the syntax for binding is `<-`
-- example
main = do
    putStrLn "Hii! What's you name?"
    name <- getLine
    putStrLn ("Hello " ++ name ++ "! Nice to meet you too!")
-- note that the following is wrong
-- putStr ("Hello " ++ getLine)
-- since getLine is not a String, it's an action that returns string that has to be "extracted" using <- before use

-- funtions vs actions
-- in constrast to languages like C/Java, where the type signature is just int -> int and any funtion is assumed to (or can) produce a side effect
-- haskell maintains a distinction, a funtions takes argument(s) and returns a result, in addition to this, an action also has a side effect

-- funtions seen till now are "free of side effects" and are called pure funtions
-- their type gives all info. we need about them
-- invoking a function on the same arguments always yield the same result
-- order of evaluation of subcompputations does not matter (haskell utilizes this in applying its lazy strategy)

-- in constrast to funtions, actions have side effeects and haskell maintains this dintinction by designating their type with `IO`
-- i.e. the presense of IO in the type indicates that the actions potentially have side effects
-- externa lstate is changed
-- and the order of computation is important (sequence of actions in the do block)

-- performing the same action on the same inputs twoice might have different result
-- in the following case the actions are same but the username might be different, so different name text will be printed/displayed
greetuser :: String -> IO ()
greetuser greeting = do
    putStr "Hii! What's your name?\n>>> "   -- for some reasons this does not work, `\n>>> ` is printed after input is given
    name <- getLine 
    putStrLn ("Hi " ++ name ++ "! " ++ greeting)

main = do
    greetuser "Welcome!"    
    greetuser "Welcome!"
    -- these two actions will print different things on screen, dependin on the name that is input by the user


-- haskells type system allows to combine pure funtions and IO actions in a safe but limited manner
-- we can use pure funtions as subroutines in IO actions, but not the other way around
-- IO is performed by an action only if it is executed within another action
-- main is where all action begins

-- examples: read a line and print it out as many times as its length
main = do
    ip <- getLine
    printoften (length ip) ip

printoften :: Int -> String -> IO ()    -- note that this in NOT a funtion, it's an action
printoften 1 str = putStrLn str         -- it HAS to be declared as an action, because it uses `putStrLn` which is an action
printoften n str = do
    putStrLn str
    printoften (n-1) str
-- if user inputs an empty string, then it will lead into a infinite loop, since n would be 0 and due n-1 will go all the way upto -infinity

-- so how do we define `printoften 0 str`?
-- we cannot define output type to `()` as it is not `IO ()`
-- so we need to promote `()` to an object of type `IO ()`
-- this is preciselly achieved by `return` funtion
-- if `v` is a value of type `a`, then `return v` is of type `IO a`
-- therefore...
printoften 0 _ = return ()
-- this is am example of the fact the a object of type `IO a` doesn't necessarily produce a side effect, it only indicates that it has the potential to create the side effect

-- another example: repeat an IO action n times
ntimes :: Int -> IO () -> IO ()
ntimes 0 _ = return ()
ntimes 0 a = do
    a
    ntimes (n-1) a

-- read and print 100 lines
main = ntimes 100 act
    where
        act = do
            ip <- getLine
            putStrLn ip

-- the funtion `readLn` reads the value of any type `a` that is instance of the typeclass `Read`
-- readLn :: Read a => IO a
-- all basic types Int, Bool, Char are instances of Read
-- basic type constructors alo presere readability
-- i.e. [Int], (Int, Char, Bool), etc. are also instances of Read
-- e.g. syntax to read an integer
ip <- readLn :: IO Int

-- example: read a list of intergres (one on each line, terminated by -1) into a list and print the list
main = do
    -- the empty list just acts as a variable that is passed around, so that it could be filled with the values we are givinh
    ls <- readList []
    -- we are reversing the list we we want to print the list in order we input
    putStrLn (show (reverse (ls)))

-- notice how the type of `readList` is `IO [Int]`, `IO` because the funtion itself causes "side effects" of inputting "Integers" (readLn funtion)
-- and [Int] because we have to extract the list of integers inputted
readList :: [Int] -> IO [Int]
readList l = do
    -- we know that Int is an instance of Read, hence the type of readLn is IO Int, as we want to take in integers
    ip <- readLn :: IO Int
    if (ip == -1) then return l else readList (ip:l)    -- last entry would be the fist in this list, hence reverse


-- only the basic baterial that should suffice for user unteraction was covered
-- THERE's a LOT MORE ON `IO` in HASKELL!!

-- this was just 
scracting the "Haskell's" surface... no... i don't think so ;)
-- see you later!

--- THE END ---
