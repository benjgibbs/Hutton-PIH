
-- My solutions to the exercieses in Hutton's Programming in Haskel
-- for ord/chr
import Char 


-- Misc

reverseWords ws = unwords(map reverse (words (reverse ws)))


-- Chapter 1

qsort [] = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs 
								where
									lhs =  [a | a <- xs,  a >= x]
									rhs =  [a | a <- xs, a < x]

-- Chapter 2

n = a `div` length xs
			where
				a = 10
				xs = [1..5]

last' xs = head(reverse xs)
last'' xs = xs !! (length xs -1)

init' xs = reverse (tail (reverse xs))
init'' xs = reverse (drop 1 (reverse xs))

-- Chapter 3
-- nothing here

--Chapter 4

halve xs = (take n xs , drop n xs)
						where
							n = length xs `div` 2

safetailA xs = if null xs then [] else tail xs

safetailB xs | null xs = []
						 | otherwise = tail xs

safetailC [] =  []
safetailC (x:xs) = xs

(?)						:: Bool -> Bool -> Bool
False ? False = False
_		  ? _ 	= True

mult = \x -> (\y -> (\z -> x*y*z))


--Chapter 5

e571 = sum [x ^2 | x <- [1..100]]

replicate' n x = [ x | _ <- [1..n]]

pythagorean z = [ (x,y,z) | x <- [1..z], y <- [1..z], z <- [1..z], x^2 + y^2 == z^2]

 
perfect x = [ y | y <- [1..x], sum(factors y) == 2*y]
						where factors x = [ y | y <- [1..x], x `mod` y == 0]

-- Caesar Cipher

let2int :: Char -> Int
let2int c = ord c - ord 'a'

int2let :: Int -> Char
int2let n = chr(ord 'a' + n)

shift 							  :: Int -> Char -> Char
shift n c | isLower c = int2let((let2int c + n) `mod` 26)
					| otherwise = c

encode 			:: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table		:: [Float]
table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,
				 0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

percent 		:: Int -> Int -> Float
percent	n m = (fromIntegral n / fromIntegral m)*100

freqs			:: String -> [Float]
freqs xs	= [percent(count x xs) n | x <- ['a'..'z']]
						where n = lowers xs


