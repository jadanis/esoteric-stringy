module Stringy
( interpretStringy
, interpret
, intr
) where

import Data.Bits
import Data.Char
import Control.Monad (join)
import Data.List (elemIndex)

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
form2 f xs x ys = form1 (f $ ord $ head ys) xs x (tail ys)
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
rep xs x ys = (join $ replicate (ord $ head ys) xs)++(tail ys)
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
grteq xs x ys = (init xs)++(if (last xs) > (head ys) then "1" else "0")++(tail ys)
-- The if-then statement
ifst' xs x ys
    | odd . ord . last $ xs = (init xs)++(removeBrace ys)
    | otherwise = (init xs)++(tail $ dropWhile (/= '}') ys)
    where removeBrace = (\st -> (takeWhile (/= '}') st)++(tail $ dropWhile (/= '}') st))
ifst xs x ys = if (elem '}' ys) then ifst' xs x ys else (if odd . ord $ last xs then (init xs)++ys else (init xs)++(tail ys))
-- The delete function
del = form3 (\x -> "")
-- Get string function (inputs '\500' - special character)
getString = form3 (\x -> x++['\500']) 
-- Subroutine function calls intr below
subr xs x ys = (fst $ intr 0 (xs,""))++ys
-- Jump to function
jump xs x ys = form3 id xs x (tail ys)
-- Equal function
eq xs x ys = (init xs)++(if (last xs) == (head ys) then "1" else "0")++(tail ys)
-- length of previous string
length' = form3 (\x -> x++[chr . length $ x])
-- Keeps the preceding string as a Subroutine
keep = del
-- call the kept function
call ext = form3 (\x -> x++ext)
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
    | x == ' ' = getString -- read string input handled elsewhere
    | x == '!' = use -- back skip function handled elsewhere
    | x == '\"' = id' -- undefined
    | x == '#' = length' -- length
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
    | x == ':' = keep -- save routine
    | x == ';' = subr 
    | x == '<' = shiftSL
    | x == '=' = eq -- equal
    | x == '>' = shiftSR
    | x == '?' = shuff
    | x == '@' = jump -- jump function handled elsewhere
    | isLetter x = id' -- Letters are non-ops 
    | x == '[' = inc
    | x == '\\' = grteq
    | x == ']' = dec
    | x == '^' = piv
    | x == '_' = rep
    | x == '`' = id' -- undefined
    | x == '{' = ifst
    | x == '|' = call -- call function
    | x == '}' = id' -- part of the ifst
    | x == '~' = rev
    | x == '\DEL' = del
    | otherwise = id' -- ASCII 0 thru 31 non ops 

-- Changes command pointer based on command
determinePointer :: Char -> Int -> Int
determinePointer x n
    | ((isLetter x) || (x == '(') || (x == '$') || (x == '^') || (x < ' ')) = (n + 1)
    | (x == '{') = (n - 1)
    | (x == '!') = (n - 2)
    | (x == ',') || (x =='\DEL') = 0
    | otherwise = n

---------------------------------------------
---------------------------------------------
-- intr is for manipulating the string     --
-- execute the command at the nth char     --
-- and continue this recursively           --
-- essentially handles all non-IO commands --
---------------------------------------------
---------------------------------------------

intr :: Int -> (String,String) -> (String,String)
intr n (prog,ext)
    | prog == "" = error "" -- error on empty string
    | (n >= length prog) || (n < 0) = intr (n `mod` (length prog)) (prog,ext) 
    | (x == '.') || (x == ' ') = (newprog,ext) -- done no call to intr
    | (x == ':') = intr m (newprog,xs)
    | otherwise = intr m (newprog,ext) -- all other commands handled here
    where x = (prog!!n) -- char at current command pointer
          xs = fst $ splitAt n prog -- string before at current command
          ys = tail . snd $ splitAt n prog -- string after current command
          newprog = (hand x) xs x ys -- string result after current command
          m = if (x == '_') || (x == ';') || (x=='|') then (length newprog) - (length ys) else (if x == '@' then ord $ head ys else determinePointer x n) -- special case for _ otherwise use determinePointer

------------------------
-- Run time functions --
------------------------

-- Contstant to indicate where input is needed
inputMarker :: Char
inputMarker = '\500'

-- Constant as string for starting a program
-- (considering intial program as user input)
start :: ([Char],String)
start = (inputMarker:[],"")

-- special index -1 if not found
elemIndex' :: (Eq a) => a -> [a] -> Int
elemIndex' x xs = case (elemIndex x xs) of
                      Just n -> n
                      Nothing -> -1

-- replace nth char with new string
inject :: Int -> String -> String -> String
inject n org inp = xs++inp++ys
    where z = splitAt n org
          xs = fst z
          ys = tail . snd $ z

-- pointer is the index of special \500 char 
-- this is -1 if not located
-- If pointer is -1 then return the program
-- newpointer is the length of input plus current pointer
-- newprogram we replace \500 with the input
-- run intr on newprogram at newpointer
-- pass this to interpretStringy
interpretStringy :: (String,String) -> IO (String,String)
interpretStringy program = do
    let pointer = elemIndex' inputMarker (fst program)
    if pointer < 0 
        then return program 
        else do
            input <- getLine
            let newpointer = (length input) + pointer
                newprogram = (inject pointer (fst program) input, snd program)
            interpretStringy $ intr newpointer newprogram

-- pass start to interpretStringy
-- bind this result
interpret = do
    (result,ext) <- interpretStringy start
    putStrLn result

-- $~[!,| :[+$|$+${$z;["| d( (r( (k( (`( (E( (-(~(z:-#+#(#'#+# #'#]#<#'#+###'#Z#'#]#'#.!)^0-0$H@~$!.!T!r!u!e!
-- ^ This should be a valid program for palindrome detection
