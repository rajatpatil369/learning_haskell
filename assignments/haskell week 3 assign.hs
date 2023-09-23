-- 1. The value of `drop 10 (takeWhile (>30) [0,4..90])` is?
-- ans
-- the `takeWhile` funtion "takes" first n elements that satisfy a condition
-- first element that does not satisfy the condition would mark termination
-- so, `takeWhile (>30) [0,4..90]` will be `[]` an empty list of `Int`, since the fist element itself is less than 30
-- so, `drop 10 (takeWhile (>30) [0,4..90])` will be also empty
-- drop 10 (takeWhile (>30) [0,4..90])
-- drop 10 []
-- []

--------------------------------------------------------------------------------

-- 2. The value of `take 5 (dropWhile (<30) [0,8..50])` is?
-- ans
-- `dropWhile` will drop first n elements that do satisfy the condition
-- so, `dropWhile (<30) [0,8..50]` will be `[32,40,48]`
-- take 5 (dropWhile (<30) [0,8..50])
-- take 5 (dropWhile (<30) [0,8,16,24,32,40,48])
-- take 5 [32,40,48]
-- [32,40,48]

--------------------------------------------------------------------------------

-- 3. The type of `f3` is?
f3 x y z = map (not . y) (zipWith (&&) x z)
-- ans
-- `zipWith` takes in a funtion of type `a -> b -> c` as its first argument
-- so, the second argument must be of type `[a]`, similarly third must be of type `[b]` and the output of `zipWith` is of type `[c]`
-- in this case, the funtion `(&&)` is of type `Bool -> Bool -> Bool`
-- so, `x` and `z` are of type `[Bool]`
-- since the output of `zipWith` is of type `[Bool]` and `not` funtion is type `Bool -> Bool`, the type of `y` should be `Bool -> Bool`
f3 :: [Bool] -> (Bool -> Bool) -> [Bool] -> [Bool]
-- side note: `zip [1..5] [1..5]` equals `[(1,1),(2,2),(3,3),(4,4),(5,5)]`

--------------------------------------------------------------------------------

-- 4. What is the value of the expression `take 5 (foldl (\y x -> [x]:y) [] [0..99])`?
-- ans
-- take 5 (foldl (\y x -> [x]:y) [] [0..99])
-- take 5 ([99]:([98]...:([2]:([1]:([0]:[])))))
-- [[99],[98],[97],[96],[95]]
-- side note: `foldl (\str ch -> [ch] ++ str) "" "tajar"` equals `"rajat"`

--------------------------------------------------------------------------------

-- 5. What is the value of the following expression?
foldl (\l x -> l ++ [x + last l]) [0] [1..5]
-- ans
-- foldl (\l x -> l ++ [x + last l]) [0] [1,2,3,4,5]
-- foldl (\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0] 1) [2,3,4,5]
-- foldl (\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0] 1) 2) [3,4,5]
-- foldl (\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0] 1) 2) 3) [4,5]
-- foldl (\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0] 1) 2) 3) 4) [5]
-- foldl (\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0] 1) 2) 3) 4) 5) []
-- ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0] 1) 2) 3) 4) 5)
-- ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ([0] ++ [1 + 0]) 2) 3) 4) 5)
-- ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ([0,1]) 2) 3) 4) 5)
-- ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0,1,3] 3) 4) 5)
-- ((\l x -> l ++ [x + last l]) ((\l x -> l ++ [x + last l]) [0,1,3,6] 4) 5)
-- ((\l x -> l ++ [x + last l]) [0,1,3,6,10] 5)
-- [0,1,3,6,10,15]

--------------------------------------------------------------------------------

-- 6. What is the position of `(4,4)` in the following infinite list?
[(i,k-i) | k <- [0,2..], i <- [0..k]]
-- ans
-- warning! the following funtion will keeping on searching forever for an element in the list if it's infinite
-- we would ASSUME that `(4,4)` exits in the given infinite list ;)
ser l x
    = let
        find :: Eq a => [a] -> a -> Int -> Int
        find [] _ _ = -1
        find (x:xs) y n
            | x == y = n
            | otherwise = find xs y (n+1)
    in find l x 0
ser [(i,k-i) | k <- [0,2..], i <- [0..k]] (4,4)
-- alright thankfully the above statement terminated
-- result was 20


-- now let's brainstorm and not cheat... the hard way
-- `[(i,j) | i <- [0..4], j <- [1..5]]` is equivalent to python's `[(i,j) for i in range(0,5) for j in range(1,6)]` and nested loops like C's `for (int i=0; i<=4; ++i) for (int j=1; j<=5; ++j) {...}`
-- keep in mind that haskell's list comprehension are not a fundamental concept, rather an elegant way of writting compositions of `map`, `filter`, and `concat`
-- so... `[(i,k-i) | k <- [0,2..], i <- [0..k]]` equals:
-- in the first iteration: k=0, i=[0..0]=[0]
-- (0,0)
-- in the second iteration: k=2, i=[0..2]=[0,1,2]
-- (0,2),(1,1),(2,0)
-- in the third iteration: k=4, i=[0..4]=[0,1,2,3,4]
-- (0,4),(1,3),(2,2),(3,1),(4,0)
-- and so on... `[(0,0),(0,2),(1,1),(2,0),(0,4),(1,3),(2,2),(3,1),(4,0),...]`
-- our target is `(4,4)`
-- you may have noticed that:
-- 1. sum of each pair's components, produced in the kth iteration, equals k
-- 2. kth iteration produces k+1 pairs
-- so 4+4 = 8, which means, the pair `(4,4)` is generated in the k=8 th iteration, and `(4,4)` is the 5th tuple from last (considering the sequence of all tuples genrated in kth iteration)
-- so we can conclude that: `pos (a,b)` is (summation of n+1 from n=0 to n=a+b, where n steps by 2) - b - 1
-- "(summation of n+1 from n=0 to n=a+b, where n steps by 2)" will give us the total number of pairs till the iteration whose `(a+b)` is actially part of
-- "subtracting b" will be like ignoring pairs after `(a+b)`, which will give us the actual position of `(a,b)`
-- remember that list postiion start with 0, so again "minus 1"

-- lets write a function for that as well :P
pos (a,b)
    | odd (a+b) = -1
    | otherwise = sum (map (+1) [0,2..(a+b)]) - b - 1

--------------------------------------------------------------------------------

-- 7. Where does (2,3) occur in the following infinite list?
[(i,k-i) | k <- [0,2..], i <- [0..k]]
-- ans
-- up for some brainstorming? decode this:
print (let i = pos (2,3); f n = if n == 0 then [] else (let q = div n 10; r = mod n 10 in r:f q) in (if i == -1 then "Does not occur" else "Position " ++ foldl (\x y -> (chr (48 + y)):x) "" (f i)))
-- Does not occur

--------------------------------------------------------------------------------

-- 8. What is the most general type for the argument `f8`?
myRepeat f8 0 x = [x]
myRepeat f8 n x = f8 (tail (myRepeat f8 (n-1) x))
-- ans
-- `tail (myRepeat f8 (n-1) x)` suggests that return type of `myRepeat` is `[a]` and since in "rule 2" of `myRepeat`, the value getting returned is the output of `f8`
-- so the output of `f8` and `myRepeat` must be compatible
-- so, the input type of `f8` is `[a]`
-- therefore the type of `f8` is `f8 :: [a] -> [a]`
-- and the type of `myRepeat` is `myRepeat :: ([a] -> [a]) -> Int -> a -> [a]`
-- or `myRepeat :: (Eq t, Num t) => ([a] -> [a]) -> t -> a -> [a]` to be specific

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

--- practice (non graded assignment)

-- 1. What is the value of the following expression?
drop 20 (filter (>50) [0,3..90])
-- ans
-- drop 20 (filter (>50) [0,3,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54..90])  -- note that this syntax is not valid
-- drop 20 [51,54..90]
-- drop 20 [51,54,57,60,63,69,72,75,78,81,84,87,90]
-- []

-- let's think this though...
-- 50 is not a multiple of 3, since `mod (5+0) 3` is not equal to 0
-- so the immediate next integer above 50 that is a multiple of 3 is 51
-- 90-51 is 39 and 39/3 is 13, hence there are 14 elements in total between (and inclusive) 51 and 90 that are multiple of 3
-- so `(drop 14)` will naturally drop all the elements of the list `[51,54..90]` because there are less than 20 elements in the list

--------------------------------------------------------------------------------

-- 2. What is the type of the following function?
f2 x y z = y:(x++z)
-- ans
f2 :: [a] -> a -> [z] -> [a]

--------------------------------------------------------------------------------

-- 3. What is the value of the following expression?
length (foldr f [0] [0..10])
    where f x y = if (x `mod` 2 == 0) then x : x + head y : y else x : y
-- ans
-- one thing we know for sure is that funtions have tigher binding over any other operator
length (foldr f [0] [0..10])
    where f x y = if (x `mod` 2 == 0) then x : x + (head y) : y else x : y
-- another thing we know is that 1st argument of `:` is `a` and 2ns is `[a]`
-- and assuming that chained `:` gets simplief from right to left
-- there also a logical explination for this:
-- `x` would be an `Int` (belonging to `[0..10]`) and y would be a `[Int` (initially y=v=[0])
-- so... `(x + head y):y` will execute first
-- then `x:((x + head y):y)`
-- considering the aobe we will rewrite the expression as:
length (foldr f [0] [0,1,2,3,4,5,6,7,8,9,10])
    where
        f x y = if (x `mod` 2 == 0) then x:((x + (head y)):y) else x:y
length (foldr f (f 10 [0]) [0,1,2,3,4,5,6,7,8,9])
length (foldr f (f 9 (f 10 [0])) [0,1,2,3,4,5,6,7,8])
length (foldr f (f 8 (f 9 (f 10 [0]))) [0,1,2,3,4,5,6,7])
length (foldr f (f 7 (f 8 (f 9 (f 10 [0])))) [0,1,2,3,4,5,6])
length (foldr f (f 6 (f 7 (f 8 (f 9 (f 10 [0]))))) [0,1,2,3,4,5])
length (foldr f (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0])))))) [0,1,2,3,4])
length (foldr f (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0]))))))) [0,1,2,3])
length (foldr f (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0])))))))) [0,1,2])
length (foldr f (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0]))))))))) [0,1])
length (foldr f (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0])))))))))) [0])
length (foldr f (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0]))))))))))) [])
length (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 (f 10 [0])))))))))))
length (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 (f 9 [10,10,0]))))))))))
length (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 (f 8 [9,10,10,0])))))))))
length (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 (f 7 [8,17,9,10,10,0]))))))))
length (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 (f 6 [7,8,17,9,10,10,0])))))))
length (f 0 (f 1 (f 2 (f 3 (f 4 (f 5 [6,13,7,8,17,9,10,10,0]))))))
length (f 0 (f 1 (f 2 (f 3 (f 4 [5,6,13,7,8,17,9,10,10,0])))))
length (f 0 (f 1 (f 2 (f 3 [4,9,5,6,13,7,8,17,9,10,10,0]))))
length (f 0 (f 1 (f 2 [3,4,9,5,6,13,7,8,17,9,10,10,0])))
length (f 0 (f 1 [2,5,3,4,9,5,6,13,7,8,17,9,10,10,0]))
length [0,1,1,2,5,3,4,9,5,6,13,7,8,17,9,10,10,0]
18

--------------------------------------------------------------------------------

-- 4. What is the value of the following expression?
take 5 (filter (<30) [0,8..100])
-- and
take 5 (filter (<30) [0,8,16,24,32..100])
take 5 [0,8,16,24]
[0,8,16,24]
