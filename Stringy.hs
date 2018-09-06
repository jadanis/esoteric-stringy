

import Data.Bits
import Data.Char
import Control.Monad (join)

-- map function over a string 
applyTo :: (Int -> Int) -> String -> String
applyTo f = map (chr . (`mod` 128) . f . ord)

-- left and right bit shifts
(.>) :: Bits a => a -> a
(.>) = flip shiftR 1
(<.) :: Bits a => a -> a
(<.) = flip shiftL 1

-----------------------------------------------------
-- The following is a list of the Stingy functions --
-- (and some helper functions as well)             --
-- determined by the Char in the string.           --
-- They are kept in a master list of functions to  --
-- be called as needed.                            --
-- These have the form:                            --
-- (String -> Char -> String -> String)            --
-----------------------------------------------------

-- the general forms these functions take
-- used to reduce typing later on and show functions in point-free form
form0 :: (Int -> Int) -> String -> Char -> String -> String
form0 f xs x ys = (applyTo f xs)++[x]++ys
form1 :: (Int -> Int) -> String -> Char -> String -> String
form1 f xs x ys = (applyTo f xs)++ys
form2 :: (Int -> Int -> Int) -> String -> Char -> String -> String
form2 f xs x ys = (applyTo (f $ ord $ head ys) xs)++(tail ys)
form3 :: (String -> String) -> String -> Char -> String -> String
form3 f xs x ys = (f xs)++ys

-- The identity function
id' = form0 id
inc = form1 succ
-- The decrement function
dec = form1 pred
-- The shift left function
shiftSL = form1 (<.)
-- The shift right function
shiftSR = form1 (.>)
-- The append digit function
appendDigit :: Int -> String -> Char -> String -> String
appendDigit n = form1 ((+n) . (*10))
-- The modulo function
mod' = form2 (flip mod)
-- The addition function
plus = form2 (+)
-- The subtraction function
minus = form2 subtract
-- The multiplication function
mult = form2 (*)
-- the divistion function
divi = form2 (flip div)
-- The repeat fucntion
rep xs x ys = (join $ replicate (ord $ head ys) xs)++ys
-- The pivot function
piv xs x ys = (init xs)++[head ys]++[last xs]++(tail ys)
-- The throw forward function
front xs x ys = [head ys]++xs++(tail ys)
-- The throw back function
back xs x ys = xs++(tail ys)++[head ys]
-- The reverse function
rev = form3 reverse
-- The shuffle function
shuff' :: String -> String
shuff' xs
    | odd n = (zipple (splitAt (s+1) xs))++[x']
    | otherwise = zipple (splitAt s xs)
    where n = length xs
          s = n `div` 2
          x' = last . fst $ splitAt (s + 1) xs
          zipple (p,q) = foldl (\acc (x,y) -> acc++[x,y]) "" $ zip p q

shuff = form3 shuff'
-- The terminate funcion
end = form1 id





grteq xs x ys = (init xs)++((if (ord (last xs)) >= (ord (head ys)) then (chr 1) else (chr 0)):[])++(tail ys)



comlist = [id',id',id',id',id',mod',id',id',front,back,mult,plus,id',minus,end,divi,(appendDigit 0),(appendDigit 1),(appendDigit 2),(appendDigit 3),(appendDigit 4),(appendDigit 5),(appendDigit 6),(appendDigit 7),(appendDigit 8),(appendDigit 9),id',id',shiftSL,id',shiftSR,shuff,id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',inc,grteq,dec,piv,rep,id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',rev,id']

com x = comlist!!((ord x) - 32)
hand xs x ys = (com x) xs x ys
lett x = ((n > 64) && (n < 91)) || ((n > 96) && (n < 123)) where n = ord x
split_ x n = map (x!!) [0..(n-1)]
_split x n = map (x!!) [(n+1)..((length x)-1)]

intr n x
    |length x == 0 = "" 
    |(n >= length x) || (n < 0) = intr (n `mod` (length x)) x 
    |(x!!n)=='.' = xs++ys 
    |(x!!n)==';' = intr n ((intr 0 xs)++ys)
    |(x!!n)=='@' = intr (ord (head ys)) (xs++(tail ys))
    |(x!!n)=='#' = intr 0 (xs++ys)
    |(x!!n)=='$' = intr (n+1) (xs++ys)
    |(x!!n)=='!' = intr (n-1) (xs++ys)
    |(x!!n)=='('  = intr (n+1) z
    |(x!!n)=='^' = intr (n+1) z
    |(x!!n)=='_' = intr m z
    |lett (x!!n) = intr (n+1) x 
    |otherwise = intr n z
    where xs = split_ x n
          ys = _split x n
          z = hand xs (x!!n) ys
          m = (length z) - (length ys)

intToSty' n
    |n < 128 = str
    |otherwise = (intToSty' (quot n 128))++str
    where str = (chr . (`mod` 128) $ n):[]

intToSty = do
    line <- getLine
    let numb = intToSty' . read $ line
    return numb

removeItem _ [] = []
removeItem x (y:ys) | x==y = ys
    | otherwise = y:(removeItem x ys)

removeBefore' x (y:ys) | x==y = ys
                       | otherwise = removeBefore' x ys
removeBefore x (y:ys) = if (elem x (y:ys)) then (removeBefore' x (y:ys)) else ys

ifst xs x ys
    |odd . ord . last $ xs = xs++(removeItem '}' ys)
    |otherwise = xs++zs
    where zs = removeBefore '}' ys

