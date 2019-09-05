-- Peter Fredatovich

import Data.List
-- import Data.ByteString 
import Data.Word

-- import System.IO
-- import Data.Char

--A function that extracts a slice from a list of integers

sliceA :: [Int] -> Int -> Int -> [Int]
sliceA [] _ _ = []
sliceA (h:t) x y  
 | y == 0 = [h] 
 | x < 0 = error "First index cannot be negative (i < 0)!"
 | y < 0 = error "Second index cannot be negative (i < 0)!"
 | x > y = error "First index cannot be greater than second index (i > k)"
 | x > 0 = sliceA t (x - 1) (y-1) 
 | otherwise = h : sliceA t x (y-1)

--A function that uses a list comprehension to extract a slice from a list of integers

sliceB :: [Int] -> Int -> Int -> [Int]
sliceB list x y
 | x < 0 = error "First index cannot be negative (i < 0)!"
 | y < 0 = error "Second index cannot be negative (i < 0)!"
 | x > y = error "First index cannot be greater than second index (i > k)"
 | otherwise = [a | a <- list, a >= list !! x, a <= list !! y]

-- MODEL
-- otherwise = [(list !! x)|x <- [x..y]]

-- A function that extracts a slice from a list of integers in either order

sliceC :: [Int] -> Int -> Int -> [Int]
sliceC [] _ _ = []
sliceC list x y  
 | x < 0 = error "First index cannot be negative (i < 0)!"
 | y < 0 = error "Second index cannot be negative (i < 0)!"
 | y > x = [a | a <- list, a >= list !! x, a <= list !! y]
 | otherwise = reverse ([a | a <- list, a <= list !! x, a >= list !! y])

-- MODEL
-- sliceC list x y
-- | x <= y = sliceA list x y
-- | otherwise = reverse(sliceA list y x)


-- A function to find the roots of a quadratic equation

quadratic :: Float -> Float -> Float -> IO ()
quadratic a b c 
 | d < 0  = putStrLn (show (a) ++ "x^2 + "  ++ show (b) ++ "x + " ++ show (c) ++ " = 0\n" ++ "Has no real roots.")
 | d == 0 = putStrLn (show (a) ++ "x^2 + "  ++ show (b) ++ "x + " ++ show (c) ++ " = 0\n" ++ "Has one real root:\n" ++ show (x))
 | d > 0  = putStrLn (show (a) ++ "x^2 + "  ++ show (b) ++ "x + " ++ show (c) ++ " = 0\n" ++ "Has two real roots:\n" ++show (y) ++ " and " ++ show (x)) 
  
 where
   x = e + sqrt d / (2 * a) 
   y = e - sqrt d / (2 * a)
   d = b * b - 4 * a * c
   e = - b / (2 * a) 

fun n 0 =0
fun n 2 = 2*2
fun n m = (fun n (m-1)) + m*2

{- MODEL ANSWER
quadratic :: Int -> Int -> Int -> IO ()
quadratic a b c 
  | d > 0 = do putStrLn(equation a b c)
               putStrLn "Has two real roots"
               putStrLn (root a b c 2)
  | d == 0 = do putStrLn(equation a b c)
               putStrLn "Has one real roots"
               putStrLn (root a b c 1)
  | d < 0 = do putStrLn(equation a b c)
               putStrLn "Has no real roots"
where d = ((b^2 - 4 *(d*c))

equation :: Int -> Int -> Int -> String
equation a b c = (show a) ++ "x^2 + " ++
                 (show b) ++ "x + " ++
                 (show c) ++ " = 0"

root :: Int -> Int -> Int -> Int -> String
root a b c 1 = showx
  where 
     x = (-bn + sqrt(bn^2 - 4 *(an*cn)))/(2*an)
     an = (fromIntegral a)
     bn = (fromIntegral b)
     cn = (fromIntegral c)

root a b c 2 = show x show x1
  where 
     x = (-bn + sqrt(bn^2 - 4 *(an*cn)))/(2*an)
     x1 = (-bn - sqrt(bn^2 - 4 *(an*cn)))/(2*an)
     an = (fromIntegral a)
     bn = (fromIntegral b)
     cn = (fromIntegral c) --}



type Encode = (Char,Int)

--  An RLE encoding function
encodeA :: Eq a  => [a] -> [(a,Int)]
encodeA list = [(head x, length x) | x <- group list]

-- An RLE decoding function
decodeA :: [Encode] -> String
decodeA list = concat (map func list)
    where
    func (n, x) = replicate x n

-- Pack a char into a tuple
packtuple :: Char -> Encode
packtuple x = (x,1)


-- Combine a tuple into a list
combine :: Eq a => (a,Int) -> [(a,Int)]  -> [(a,Int)]
combine (a,b) ( (c,d) : tail )
  | a == c = (a, b+d) : tail
  | otherwise = (a,b) : (c,d) : tail

-- EncodeB
--
-- encodeB :: Eq a  =>  [a] -> [(Encode)]
-- encodeB = map (foldr combine. packtuple)

-- RLE with 8 bit
encode :: Eq a => [a] -> [(a,Int)] 
encode = map rle . group
  where rle x = (head x, length x) 
 