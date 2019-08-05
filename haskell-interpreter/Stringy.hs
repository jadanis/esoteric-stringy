module Stringy
( run
, runStringy
, interpret
, ProgramState
) where

import Data.Bits
import Data.Char
import Control.Monad (join)
import Data.List (elemIndex)

type Pointer = Int
type Operator = Char
type Program = String
type ProgramState = IO (Program,Pointer,Program,Bool)

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

form0 :: (Int -> Int) -> Pointer -> Program -> Program
form0 f pt prog = (applyTo f xs)++[x]++ys
    where xs = take pt prog
          x = prog !! pt
          ys = drop (pt + 1) prog

form1 :: (Int -> Int) -> Pointer -> Program -> Program
form1 f pt prog = (applyTo f xs)++ys
    where xs = take pt prog
          x = prog !! pt
          ys = drop (pt + 1) prog

form2 :: (Int -> Int -> Int) -> Pointer -> Program -> Program
form2 f pt prog = (applyTo n_f xs)++(tail ys)
    where xs = take pt prog
          x = prog !! pt
          ys = drop (pt + 1) prog
          n_f = f $ ord $ head ys

form3 :: (String -> String) -> Pointer -> Program -> Program
form3 f pt prog = (f xs)++ys
    where xs = take pt prog
          x = prog !! pt
          ys = drop (pt + 1) prog

hand b mv f state = do
    (prog,pt,ext,r) <- state
    let newprog = b f pt prog
    let npt = mv pt
    return (newprog,npt,ext,r)

stay :: Pointer -> Pointer
stay = id
fwr :: Pointer -> Pointer
fwr = (+) 1
bck :: Pointer -> Pointer
bck = subtract 2

hand0 :: (Int -> Int) -> ProgramState -> ProgramState
hand0 = hand form0 fwr

hand1 :: (Int -> Int) -> ProgramState -> ProgramState
hand1 = hand form1 stay

hand2 :: (Int -> Int -> Int) -> ProgramState -> ProgramState
hand2 = hand form2 stay

hand3 :: (String -> String) -> ProgramState -> ProgramState
hand3 = hand form3 stay


----------------------------------------------
-- With the except of appendDigit, the follo -
-- wing are :: ProgramState -> ProgramState --
-- These make up the possible opeartions fo --
-- r Stringy.                               --
----------------------------------------------

-- The identity function
id' = hand0 id

-- The increment function
inc = hand1 succ

-- The decrement function
dec = hand1 pred

-- The shift left function
(<.) :: Bits a => a -> a
(<.) = flip shiftL 1
shiftSL = hand1 (<.)

-- The shift right function
(.>) :: Bits a => a -> a
(.>) = flip shiftR 1
shiftSR = hand1 (.>)

-- The append digit function
appendDigit :: Int -> (ProgramState -> ProgramState)
appendDigit n = hand1 ((+n) . (*10))

-- The modulo function
mod' = hand2 (flip mod)

-- The addition function
plus = hand2 (+)

-- The subtraction function
minus = hand2 subtract

-- The multiplication function
mult = hand2 (*)

-- the divistion function
divi = hand2 (flip div)

-- The repeat fucntion
rep state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = (join $ replicate (ord $ head ys) xs)++(tail ys)
    let npt = (length xs) * (ord $ head ys)
    return (newprog,npt,ext,r)

-- The pivot function
piv state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = if xs == "" then (if (length ys) > 1 then [last ys]++(init . tail $ ys)++[head ys] else ys) else((init xs)++[head ys]++[last xs]++(tail ys))
    return (newprog,pt+1,ext,r)

-- The throw forward function
front state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = [head ys]++xs++(tail ys)
    return (newprog,pt + 1,ext,r)

-- The throw back function
back state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = xs++(tail ys)++[head ys]
    return (newprog,pt,ext,r)

-- The reverse function
rev = hand3 reverse

-- The shuffle function
shuff' :: String -> String
shuff' xs
    | odd n = (zipple (splitAt (s+1) xs))++[x']
    | otherwise = zipple (splitAt s xs)
    where n = length xs
          s = n `div` 2
          x' = last . fst $ splitAt (s + 1) xs
          zipple (p,q) = foldl (\acc (x,y) -> acc++[x,y]) "" $ zip p q

shuff = hand3 shuff'

-- The greater than or equal function
grteq state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = if xs == "" then "0"++(tail ys) else (init xs)++(if (last xs) > (head ys) then "1" else "0")++(tail ys)
    return (newprog,pt,ext,r)

-- The if-then statement
ifst' xs x ys
    | odd . ord . last $ xs = (init xs)++(removeBrace ys)
    | otherwise = (init xs)++(tail $ dropWhile (/= '}') ys)
    where removeBrace = (\st -> (takeWhile (/= '}') st)++(tail $ dropWhile (/= '}') st))
ifst state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = if xs == "" then (ifst' "0" x ys) else (if (elem '}' ys) then ifst' xs x ys else (if odd . ord $ last xs then (init xs)++ys else (init xs)++(tail ys)))
    return (newprog,pt - 1,ext,r)

-- The delete function
del = hand form3 (\x -> 0) (\x -> "")


-- Get string function (inputs '\500' - special character)
getString state = do
    (prog,pt,ext,r) <- state
    inp <- getLine
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = xs ++ inp ++ ys
    let npt = length (xs ++ inp)
    return (newprog,npt,ext,r)


-- Subroutine function calls intr below
subr state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    res <- runStringy $ return (xs,0,ext,True)
    let newprog = res ++ ys
    let npt = length res
    return (newprog,npt,ext,r)


-- Jump to function
jump state = do
    (prog,pt,ext,r) <- state
    inp <- getLine
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = xs ++ (tail ys)
    let npt = ord $ head ys
    return (newprog,npt,ext,r)


-- Equal function
eq state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = if xs == "" then "0"++(tail ys) else (init xs)++(if (last xs) == (head ys) then "1" else "0")++(tail ys)
    return (newprog,pt,ext,r)

-- length of previous string
length' = hand form3 fwr (\x -> x++[chr . (`mod` 128) . length $ x])

-- Keeps the preceding string as a Subroutine
keep state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    return (ys,0,xs,r)

-- call the kept function
call state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = xs ++ ext ++ ys
    let npt = pt + (length ext)
    return (newprog,npt,ext,r)


-- operates the function on only a single char instead of the whole string
op1 state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    (res,pti,_,_) <- interpret $ return ([last xs]++ys,1,ext,r)
    let newprog = (init xs)++res
    let npt = pt + pti - 1
    return (newprog,npt,ext,r)

-- Uses up the current char but changes nothing otherwise
use = hand form1 fwr id

-- Uppercase 
upper = hand3 (map toUpper)

-- Lowercase
lower = hand3 (map toLower)

ret = hand form3 (\x -> 0) id

back_skip = hand form3 bck id

-- The terminate funcion
-- Same as use just another name for clarity
end state = do
    (prog,pt,ext,r) <- state
    let xs = take pt prog
    let x = prog !! pt
    let ys = drop (pt + 1) prog
    let newprog = xs ++ ys
    return (newprog,pt,ext,False)

---------------------------------------------------------
-- hand (for handle) calls the string function based   --
-- on the current character selected. Originally was a --
-- list of functions here using guards                 --
-- Some of this behavior is undefined yet - 09/06/18   --
---------------------------------------------------------
getOp :: Operator -> (ProgramState -> ProgramState)
getOp x
    | x == ' ' = getString -- read string input handled elsewhere
    | x == '!' = back_skip -- back skip function handled elsewhere
    | x == '\"' = upper -- undefined
    | x == '#' = length' -- length
    | x == '$' = use -- skip function handled elsewhere
    | x == '%' = mod'
    | x == '&' = id' -- undefined
    | x == '\'' = lower  -- undefined
    | x == '(' = front
    | x == ')' = back
    | x == '*' = op1
    | x == '+' = plus
    | x == ',' = ret
    | x == '-' = minus
    | x == '.' = end
    | x == '/' = divi
    | isDigit x = (appendDigit $ read [x])
    | x == ':' = keep 
    | x == ';' = subr
    | x == '<' = shiftSL
    | x == '=' = eq 
    | x == '>' = shiftSR
    | x == '?' = shuff
    | x == '@' = jump 
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

interpret :: ProgramState -> ProgramState
interpret state = do
    (prog,pt,ext,r) <- state
    let npt = pt `mod` (length prog)
    let op = prog !! npt
    (getOp op) $ return (prog,npt,ext,r)

initialState :: ProgramState
initialState = return ("!.0+,%}^{!^^!A-*C_$A-*K/$$(`+#+#~",0,"",True)

runStringy :: ProgramState -> IO Program
runStringy state = do
    (prog,pt,ext,r) <- state
    if r then
        runStringy $ interpret state
    else
        return prog

run :: IO ()
run = do
    output <- runStringy initialState
    putStrLn output
