module Stringy
(
intr
) where

import Data.Bits
import Data.Char
import Control.Monad (join)

-- map function over a string 
applyTo :: (Int -> Int) -> String -> String
applyTo f = map (chr . (`mod` 128) . f . ord)

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
-- The increment function
inc = form1 succ
-- The decrement function
dec = form1 pred
-- The shift left function
(<.) :: Bits a => a -> a
(<.) = flip shiftL 1
shiftSL = form1 (<.)
-- The shift right function
(.>) :: Bits a => a -> a
(.>) = flip shiftR 1
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
-- The greater than or equal function
grteq xs x ys = (init xs)++(if (last xs) >= (head ys) then ['1'] else ['0'])++(tail ys)
-- The if-then statement
ifst xs x ys
    | odd . ord . last $ xs = (init xs)++(removeBrace ys)
    | otherwise = (init xs)++(tail $ dropWhile (/= '}') ys)
    where removeBrace = (\st -> (takeWhile (/= '}') st)++(tail $ dropWhile (/= '}') st))
-- Uses up the current char but changes nothing otherwise
use = form1 id
-- The terminate funcion
-- Same as use just another name for clarity
end = use

---------------------------------------------------------
-- hand (for handle) calls the string function based   --
-- on the current character selected. Originally was a --
-- list of functions here using guards                 --
-- Some of this behavior is undefined yet - 09/06/18   --
---------------------------------------------------------
hand :: Char -> (String -> Char -> String -> String)
hand x
    | x == ' ' = id' -- read string input handled elsewhere
    | x == '!' = use -- back skip function handled elsewhere
    | x == '\"' = id' -- undefined
    | x == '#' = id' -- read number input handled elsewhere
    | x == '$' = use -- skip function handled elsewhere
    | x == '%' = mod'
    | x == '&' = id' -- undefined
    | x == '\'' = id' -- undefined
    | x == '(' = front
    | x == ')' = back
    | x == '*' = mult
    | x == '+' = plus
    | x == ',' = use -- set command pointer to 0, handled elsewhere
    | x == '-' = minus
    | x == '.' = end
    | x == '/' = divi
    | isDigit x = (appendDigit $ read [x])
    | x == ':' = id' -- undefined
    | x == ';' = id' -- subroutine function handled elsewhere
    | x == '<' = shiftSL
    | x == '=' = id' -- undefined
    | x == '>' = shiftSR
    | x == '?' = shuff
    | x == '@' = id' -- jump function handled elsewhere
    | isLetter x = id' -- Letters are non-ops 
    | x == '[' = inc
    | x == '\\' = grteq
    | x == ']' = dec
    | x == '^' = piv
    | x == '_' = rep
    | x == '`' = id' -- undefined
    | x == '{' = ifst
    | x == '|' = id' -- undefined
    | x == '}' = id' -- part of the ifst
    | x == '~' = rev
    | x == '\DEL' = id' -- undefined
    | otherwise = id' -- ASCII 0 thru 31 non ops 

-- Changes command pointer based on command
determinePointer :: Char -> Int -> Int
determinePointer x n
    | ((isLetter x) || (x == '(') || (x == '$') || (x == '^')) = (n + 1)
    | (x == '!') = (n - 2)
    | (x == ',') = 0
    | otherwise = n

-- Most of the program interpretation can be handled here
intr :: Int -> String -> String
intr n prog
    | prog == "" = error "No program was provided!" -- error on empty string
    | (n >= length prog) || (n < 0) = intr (n `mod` (length prog)) prog 
    | x == '.' = end xs x ys -- done no call to intr
    | x == ';' = intr n ((intr 0 xs)++ys) -- call intr on xs at 0 before resuming with the whole result at n
    | x == '@' = intr (ord (head ys)) (xs++(tail ys)) -- set command point to the ord of head of ys
    | otherwise = intr m newprog -- all other commands handled here
    where x = (prog!!n) -- char at current command pointer
          xs = fst $ splitAt n prog -- string before at current command
          ys = tail . snd $ splitAt n prog -- string after current command
          newprog = (hand x) xs x ys -- string result after current command
          m = if x == '_' then (length newprog) - (length ys) else determinePointer x n -- special case for _ otherwise use determinePointer

-- handling IO functions - In progress
intToSty' n
    |n < 128 = str
    |otherwise = (intToSty' (quot n 128))++str
    where str = (chr . (`mod` 128) $ n):[]

intToSty = do
    line <- getLine
    let numb = intToSty' . read $ line
    return numb
