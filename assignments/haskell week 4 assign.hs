-- 1. Define a function `f1 :: [Int] -> [Int]` which takes a list `l` of non-negative numbers as input, and replaces each n in `l` by `3*n` if n is a power of 3, and by 0 if it is not a power of 3.
-- Examples:
--   f1 [] = []
--   f1 [1] = [3]
--   f1 [1, 2, 3] = [3, 0, 9]
--   f1 [0, 2, 4, 6] = [0, 0, 0, 0]

powOf3 :: Int -> Bool
powOf3 1 = True
powOf3 x
    | x <= 0 = False
    | mod x 3 == 0 = powOf3 (div x 3)
    | otherwise = False

f1 :: [Int] -> [Int]
f1 [] = []
f1 (n:ns) = (if powOf3 n then 3*n else 0) : f1 ns

-- or
f1v1 = map ((*3) . (\x -> (if powOf3 x then 3*x else 0)))

--------------------------------------------------------------------------------

-- 2. For a list `l`, define S(l) to be the set of all indices `i` of `l` (remember that indices start from 0) such that `l!!i > l!!(i+1)`. Define a function `f2 :: [Int] -> [Int]` which takes a nonempty list `l` of integers as input and outputs a S(l) in order.
-- Examples:
--   f2 [] = []
--   f2 [1] = []
--   f2 [1, 2, 3, 2, 1] = [2, 3]
--   f2 [1, 2, 3, 4, 5, 6] = []

f2 = f 0    -- (\l -> f l 0)
    where
        f :: Ord a => Int -> [a] -> [Int]
        f _ [] = []
        f _ [x] = []
        f i (x:y:xs)
            | x > y = i : f (i+1) (y:xs)
            | otherwise = f (i+1) (y:xs)

--------------------------------------------------------------------------------

-- 3. Define a function `f3 :: [Int] -> [Int]` that removes adjacent duplicates, i.e. if the same element occurs n times contiguously, we retain only one copy.
-- Examples:
--   f3 [1, 1, 1, 2, 2, 3, 3, 3, 3] = [1, 2, 3]
--   f3 [1, 2, 1, 2, 3, 1, 1, 2, 2] = [1, 2, 1, 2, 3, 1, 2]

f3 :: [Int] -> [Int]
f3 [] = []
f3 [x0] = [x0]
f3 (x0:x1:xs)
    | x0 == x1 = f3 (x1:xs)
    | otherwise = x0 : f3 (x1:xs)

--------------------------------------------------------------------------------

-- 4. Define a function `f4 :: [Int] -> [[Int]]` that partitions the list into all its upruns. An uprun is a maximal non-decreasing segment of the given list. 
-- Examples:
--   f4 [] = []
--   f4 [5] = [[5]]
--   f4 [1, 2, 3, 4, 5] = [[1,2,3,4,5]]
--   f4 [1, 2, 3, 4, 5, 6, 5, 4, 3, 2, 1] = [[1,2,3,4,5,6],[5],[4],[3],[2],[1]]

f4 :: [Int] -> [[Int]]
f4 = f []
    where
        f :: [Int] -> [Int] -> [[Int]]
        f _ [] = []
        f p [x] = [p ++ [x]]
        f p (x:y:xs)    -- p for partition
            | x <= y = f (p ++ [x]) (y:xs)
            | otherwise = (p ++ [x]) : f [] (y:xs)

-- or
f4v1 xs = let (uprun, rest) = breakDecreasing xs in uprun : f4 rest
    where
        breakDecreasing :: [Int] -> ([Int], [Int])
        breakDecreasing [] = ([], [])
        breakDecreasing [x] = ([x], [])
        breakDecreasing (x:y:xs)
          | x <= y = let (uprun, rest) = breakDecreasing (y:xs) in (x : uprun, rest)
          | otherwise = ([x], y:xs)
