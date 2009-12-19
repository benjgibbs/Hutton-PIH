
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

(?)            :: Bool -> Bool -> Bool
False  ? False = False
_       ? _   = True

mult = \x -> (\y -> (\z -> x*y*z))


--Chapter 5

e571 = sum [x ^2 | x <- [1..100]]

replicate' n x = [ x | _ <- [1..n]]

pyths z = [ (x,y,z) | x <- [1..z], y <- [1..z], z <- [1..z], x^2 + y^2 == z^2]

 
perfect x = [ y | y <- [1..x], sum(factors y) == 2*y]
            where factors x = [ y | y <- [1..x], x `mod` y == 0]

problem575 = [(x,y)|x <- [1..3], y<- [4..6]]
problem575b = concat [[(x,y) | y <- [4,5,6]] | x <- [1,2,3]]

find        :: Eq a => a -> [(a,b)] ->[b]
find k t    = [v | (k',v) <- t,k==k']

positions' ::    Eq a => a -> [a] -> [Int]
positions' a xs = find a (zip xs [0..])

scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- (zip xs ys)]

-- Upper case Caesar Cipher
-- TODO include upper case letters in frequency analysis
letters = concat [['A'..'Z'],['a'..'z']]

let2int :: Char -> Int
let2int c = head [y|(x,y)<-(zip letters [0..]),x==c]

int2let :: Int -> Char
int2let n = head (drop n letters)

encode      :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
            where
                shift n c | isAlpha c = int2let((let2int c+n) `mod` (length letters))
                          | otherwise = c

freqs      :: String -> [Float]
freqs xs  = [percent ((count x xs)+(count (toUpper x) xs)) (length xs) 
        | x <- ['a'..'z']]
            where                   
                count k cs = length[c | c <- cs, c==k]
                percent  n m = (fromIntegral n / fromIntegral m)*100

chisqr       :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e) ^ 2) / e |(o,e) <- zip os es]

positions      :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x==x']
                 where
                    n = length xs  - 1

crack   :: String -> String
crack xs = encode(-factor) xs
    where
        factor = head ( positions (minimum chitab) chitab )
        chitab =[chisqr (rotate n table') table | n <- [0..25]]
        table = [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,6.7,7.5,1.9,
                 0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]
        table' = freqs xs
        rotate n xs = drop n xs ++ take n xs
        

--Chapter 6

exp'         :: Int -> Int -> Int
exp'  x 0     = 1
exp'  x (y+1) = x * (x `exp'` y)

length' [] = 0
length' xs = 1 + (length' (tail xs))

drop' 0 xs = xs
drop' n [] = [] 
drop' (n+1) xs = drop' n (tail xs)

len xs = foldr( (+) 1)

and' [True] = True
and' (True:xs) = and' xs
and' (False:xs) = False

concat' [] = []
concat' (xs:ys) = xs ++ concat ys

rep 0 x = []
rep (n+1) x = [x] ++ rep n x

nth 1 (x:xs) = x
nth (n+1) (x:xs) = nth n xs

elem' c [] = False
elem' c (x:xs) | c == x = True 
               | otherwise = elem' c xs

merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = [x] ++ merge xs (y:ys)
                    | otherwise = [y] ++ merge (x:xs) ys

msort [] = [] 
msort [x] = [x]
msort xs = merge (msort (fst h)) (msort (snd h))
            where
                h = halve xs

sum' = foldr (+) 0




