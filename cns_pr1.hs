-- cryptography and network security tec

-- write a program to demonstrate shift cypher and mono alphabetic cypher algorithm

-- shift cypher
-- y = f(x) = (x + k) % 26
-- where,
--      `y` is the cypher text,
--      `x` is plain text,
--      `k` is the key,
--      `%` is the modulo operator (gives reminder of a division operation)


char2int :: Char -> Int
char2int c = head [i | (i,(upper,lower)) <- zip [0..25] (zip ['A'..'Z'] ['a'..'z']), upper == c || lower == c]

int2char :: Int -> Char
int2char n = head [c | (i,c) <- zip [0..25] ['a'..'z'], i == n]

shift_cipher :: Int -> String -> String
shift_cipher _ "" = ""
shift_cipher key plain_text
    = map f plain_text
    where
        f :: Char -> Char
        f c = int2char (mod (char2int c + key) 26)

shift_decipher :: Int -> String -> String
shift_decipher _ "" = ""
shift_decipher key cipher_text
    = map f cipher_text
    where
        f :: Char -> Char
        f c = int2char (mod (char2int c - key) 26)
        

-- monoalphabetic cipher
-- each alphabet of the plain text is mapped to another alphabet within a lookup table

lookup_value :: [(Char,Char)] -> Char -> Char
lookup_value ((a,b):xs) c
    | c == a = b
    | otherwise = lookup_value xs c

monoalphabetic_cipher :: [(Char,Char)] -> String -> String
monoalphabetic_cipher _ "" = ""
monoalphabetic_cipher lut plain_text
    = map (lookup_value (lut ++ [(i,i) | i <- ['A'..'Z'] ++ ['a'..'z'], not (f lut i)])) plain_text
    where
        f :: [(Char,Char)] -> Char -> Bool
        f [] _ = False
        f ((c,_):xs) x = c == x || f xs x

swap_key_value :: [(Char,Char)] -> [(Char,Char)]
swap_key_value [] = []
swap_key_value ((k,v):xs) = (v,k):(swap_key_value xs)

monoalphabetic_decipher :: [(Char,Char)] -> String -> String
monoalphabetic_decipher lut cipher_text = monoalphabetic_cipher (swap_key_value lut) cipher_text

main :: IO ()
main =  do
    print (shift_cipher 7 "rajat")
    print (shift_decipher 7 "yhqha")
    print (monoalphabetic_cipher [('r','f'),('a','q'),('j','u')] "rajat")
    print (monoalphabetic_decipher [('r','f'),('a','q'),('j','u')] "fquqt")
