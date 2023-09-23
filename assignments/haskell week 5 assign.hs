-- 1. Define a function `subSeq :: String -> String -> Bool` which checks whether the first argument is a subsequence of the second. A subsequence is obtained by deleting some letters in a string and retaining the other characters in the same order as in the original string.
-- Test cases:
-- subSeq "ab" "abc" = True
-- subSeq "ab" "acb" = True
-- subSeq "ab" "bca" = False
-- subSeq ""   "bea" = True
-- subSeq "ba" "ba"  = True

subSeq :: String -> String -> Bool
subSeq "" _ = True
subSeq _ "" = False
subSeq (x:xs) (y:ys)
    | x == y = subSeq xs ys
    | otherwise = subSeq (x:xs) ys

--------------------------------------------------------------------------------

-- 2. Define a function `subWord :: String -> String -> Bool` which checks whether the first argument is a subword of the second. A subword is obtained by deleting some number (possibly 0) of letters at the left end and right end in a string and retaining the other characters in the same order.
-- Test cases:
-- subWord "ab" "abc" = True
-- subWord "ab" "acb" = False
-- subWord "ca" "bca" = True
-- subWord ""   "bea" = True
-- subWord "ba" "ba"  = True

subWord :: String -> String -> Bool
subWord sw w = f sw w
    where
        f "" _ = True
        f _ "" = False
        f (x:xs) (y:ys) = f (if x == y then xs else sw) ys

-- side note: `subword@(x:xs)` this syntax will "bind" the pattern `(x:xs)` to the name `subword`, you can refer to the pattern later in the funtion

--------------------------------------------------------------------------------

-- 3. A two-dimensional matrix can be represented as a list of rows, each row itself being a list of elements. So in general it is of type `[[a]]`. Not every list of lists is a matrix, though. For instance, `[[1,2,3], [], [2,4]]` is a list of three lists, each of a different size.

-- (a) Define a function `isMatrix :: [[a]] -> Bool` that checks if a list of lists is a valid matrix (nonzero number of rows, each of the same nonzero length).
-- Test cases:
-- isMatrix [] = False
-- isMatrix [[],[],[]] = False
-- isMatrix [[2,3], [4,5], [6,7]] = True 
-- isMatrix [[2,3,4,5,6,7]] = True

isMatrix :: [[a]] -> Bool
isMatrix [] = False
isMatrix (x:xs) = f (length x) xs
    where
        f 0 _ = False
        f n [] = True
        f n (y:ys)
            | n == length y = f n ys
            | otherwise = False

-- or
isMatrix_v1 [] = False
isMatrix_v1 mat = let x = length (head mat) in and (map ((== x) . length) (tail mat))

-- or
isMatrix_v2 [] = False
isMatrix_v2 mat = let x = length (head mat) in all ((== x) . length) (tail mat)
-- ... in all (== x) (map length (tail mat))

-- or (may be the least ineffecient implementation, but works...)
isMatrix_v3 [] = False
isMatrix_v3 mat = if foldl1 (\x y -> if x == y && x /= 0 && y /= 0 then x else -1) (map length mat) /= -1 then True else False


-- (b) A square matrix is one where the number of rows is equal to the number of columns. Define a function `isSquareMatrix :: [[a]] -> Bool` that checks if a list of lists is a square matrix.
-- Test cases:
-- isSquareMatrix [] = False
-- isSquareMatrix [[]] = False
-- isSquareMatrix [[1]] = True
-- isSquareMatrix [[1,2,3],[4,5,6],[7,8,9]] = True
-- isSquareMatrix [[1,2,3,4],[5,6,7,8],[9,10,11,12]] = False

isSquareMatrix :: [[a]] -> Bool
isSquareMatrix [] = False
isSquareMatrix mat = let n = length mat in all ((== n) . length) mat


-- (c) Two matrices are addable if they have the same number of rows and same number of columns. Define a function `addable :: [[a]] -> [[a]] -> Bool` that checks if two matrices are addable.
-- Test cases: 
-- addable [[1,2],[3,4]] [[1,2],[3,4]] = True
-- addable [[1,2],[3,4]] [[5,6,7],[8,9,10]] = False
-- addable [[1,2],[3,4]] [[1,2],[3,4],[3,4]] = False

type Matrix a = [[a]]

{-
data Matrix a = Matrix [[a]]

instance Show a => Matrix a     -- error: ‘show’ is not a (visible) method of class ‘Matrix’
    where
        show mat
            | isMatrix mat =
                let l_max = maximum (map (length . show) (concat mat))
                in "[[" ++ foldl1 (\x y -> x ++ "],\n [" ++ y) (map ((foldl1 (\x y -> x ++ ", " ++ y)) . (map (\x -> let str = show x; len = length str in replicate (l_max - len) ' ' ++ str))) mat) ++ "]]"
            | otherwise = "Not a Matrix"

instance Show Order
    where
        show (Order r c) = show r ++ "x" ++ show c
-}

data Order = Order {
    row :: Int,
    column :: Int
} deriving Eq

orderOf :: Matrix a -> Order
orderOf mat = Order (length mat) (length (head mat))

addable :: Matrix a -> Matrix a -> Bool
addable mat1 mat2
    | isMatrix mat1 && isMatrix mat2 = orderOf mat1 == orderOf mat2
    | otherwise = False


-- (d) Define a function `addMatrices :: [[Int]] -> [[Int]] -> [[Int]]` that computes the sum of the input matrices.
-- Test cases:
-- addMatrices [[1,2]] [[3,4]] = [[4,6]]
-- addMatrices [[1,2],[3,4]] [[1,2],[3,4]] = [[2,4],[6,8]]

addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices mat1 mat2 = zipWith (zipWith (+)) mat1 mat2


-- (e) Matrix `m1` is multiplyable with matrix `m2` if the number of columns in `m1` is the same as the number of rows in `m2`. Define a function `multiplyable :: [[a]] -> [[a]] -> Bool` that checks if matrix `m1` is multiplyable with `m2`.
-- Test cases:
-- multiplyable [[1,2,3],[4,5,6]] [[1,2],[3,4]] = False
-- multiplyable [[1,2,3],[4,5,6],[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = True

multiplyable :: [[a]] -> [[a]] -> Bool
multiplyable m1 m2 = column (orderOf m1) == row (orderOf m2)

-- (f) Define a function `multiplyMatrices :: [[Int]] -> [[Int]] -> [[Int]]` that computes the product of the input matrices.
-- Test cases:
-- multiplyMatrices [[1,2],[3,4]] [[1,2,3],[4,5,6]] = [[9,12,15],[19,26,33]]
-- multiplyMatrices [[1,2,3],[4,5,6]] [[1,2],[3,4],[5,6]] = [[22,28],[49,64]]

transpose :: [[Int]] -> [[Int]]
transpose mat = [foldr (\l -> (l!!i:)) [] mat | i <- [0..(length (head mat) - 1)]]

-- or
transpose_v1 ([]:_) = []
transpose_v1 mat = (map head matrix) : transpose (map tail matrix)

multiplyMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multiplyMatrices m1 m2 = let m2' = transpose m2 in [[foldl1 (+) (zipWith (*) r r') | r' <- m2'] | r <- m1]
