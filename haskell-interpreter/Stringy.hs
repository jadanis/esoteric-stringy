module Stringy
( run
, runStringy
, interpret
, ProgramState
, debug
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
gobble :: Pointer -> Program -> (Program,Operator,Program)
gobble pt prog = (xs,x,ys)
    where xs = take pt prog
          x = prog !! pt
          ys = drop (pt + 1) prog

form0 :: (Int -> Int) -> Pointer -> Program -> Program
form0 f pt prog = (applyTo f xs)++[x]++ys
    where (xs,x,ys) = gobble pt prog

form1 :: (Int -> Int) -> Pointer -> Program -> Program
form1 f pt prog = (applyTo f xs)++ys
    where (xs,x,ys) = gobble pt prog

form2 :: (Int -> Int -> Int) -> Pointer -> Program -> Program
form2 f pt prog = (applyTo n_f xs)++(tail ys)
    where (xs,x,ys) = gobble pt prog
          n_f = f $ ord $ head ys

form3 :: (String -> String) -> Pointer -> Program -> Program
form3 f pt prog = (f xs)++ys
    where (xs,x,ys) = gobble pt prog

hand :: (p -> Pointer -> Program -> Program) -> (Pointer -> Pointer) -> p -> ProgramState -> ProgramState
hand b mv f state = do
    (prog,pt,ext,r) <- state
    let newprog = b f pt prog
    let npt = mv pt
    return (newprog,npt,ext,r)

-- Pointer shifts)
stay :: Pointer -> Pointer
stay = id
fwr :: Pointer -> Pointer
fwr = (+) 1
bck :: Pointer -> Pointer
bck = subtract 2

-- Basic ways of manipulating ProgramStates
-- some operations do not use this structure
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
-- A-Za-z among other operators
id' :: ProgramState -> ProgramState
id' = hand0 id

-- The increment function
-- Operator: `[`
inc :: ProgramState -> ProgramState
inc = hand1 succ

-- The decrement function
-- Operator: `]`
dec :: ProgramState -> ProgramState
dec = hand1 pred

-- The shift left function
-- Operator: `<`
(<.) :: Bits a => a -> a
(<.) = flip shiftL 1
shiftSL :: ProgramState -> ProgramState
shiftSL = hand1 (<.)

-- The shift right function
-- Operator: `>` 
(.>) :: Bits a => a -> a
(.>) = flip shiftR 1
shiftSR :: ProgramState -> ProgramState
shiftSR = hand1 (.>)

-- The append digit function
-- Operators: `0` thru `9`
appendDigit :: Int -> (ProgramState -> ProgramState)
appendDigit n = hand1 ((+n) . (*10))

-- The modulo function
-- Operator: `%`
mod' :: ProgramState -> ProgramState
mod' = hand2 (flip mod)

-- The addition function
-- Operator: `+`
plus :: ProgramState -> ProgramState
plus = hand2 (+)

-- The subtraction function
-- Operator: `-`
minus :: ProgramState -> ProgramState
minus = hand2 subtract

-- The multiplication function
-- Operator: deprecated
mult :: :: ProgramState -> ProgramState
mult = hand2 (*)

-- the divistion function
-- Operator: `/`
divi :: ProgramState -> ProgramState
divi = hand2 (flip div)

-- The repeat fucntion
-- Operator: `_`
rep :: ProgramState -> ProgramState
rep state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = (join $ replicate (ord $ head ys) xs)++(tail ys)
    let npt = (length xs) * (ord $ head ys)
    return (newprog,npt,ext,r)

-- The pivot function
-- Operator: `^`
piv :: ProgramState -> ProgramState
piv state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = if xs == "" then (if (length ys) > 1 then [last ys]++(init . tail $ ys)++[head ys] else ys) else((init xs)++[head ys]++[last xs]++(tail ys))
    return (newprog,pt+1,ext,r)

-- The throw forward function
-- Operator: `(`
front :: ProgramState -> ProgramState
front state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = [head ys]++xs++(tail ys)
    return (newprog,pt + 1,ext,r)

-- The throw back function
-- Operator: `)`
back state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = xs++(tail ys)++[head ys]
    return (newprog,pt,ext,r)

-- The reverse function
-- Operator: `~`
rev :: ProgramState -> ProgramState
rev = hand3 reverse

-- The shuffle function
-- Operator: `?`
shuff' :: String -> String
shuff' xs
    | odd n = (zipple (splitAt (s+1) xs))++[x']
    | otherwise = zipple (splitAt s xs)
    where n = length xs
          s = n `div` 2
          x' = last . fst $ splitAt (s + 1) xs
          zipple (p,q) = join $ [ [x,y] | (x,y) <- zip p q ]
shuff :: ProgramState -> ProgramState
shuff = hand3 shuff'

-- The greater than or equal function
-- Operator: `\`
grteq :: ProgramState -> ProgramState
grteq state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = if xs == "" then "0"++(tail ys) else (init xs)++(if (last xs) > (head ys) then "1" else "0")++(tail ys)
    return (newprog,pt,ext,r)

-- The if-then statement
-- Operator: `{`
ifst' :: Program -> Operator -> Program -> Program
ifst' xs x ys
    | odd . ord . last $ xs = (init xs)++(removeBrace ys)
    | otherwise = (init xs)++(tail $ dropWhile (/= '}') ys)
    where removeBrace = (\st -> (takeWhile (/= '}') st)++(tail $ dropWhile (/= '}') st))
ifst :: ProgramState -> ProgramState
ifst state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = if xs == "" then (ifst' "0" x ys) else (if (elem '}' ys) then ifst' xs x ys else (if odd . ord $ last xs then (init xs)++ys else (init xs)++(tail ys)))
    return (newprog,pt - 1,ext,r)

-- The delete function
-- Operator: `\DEL`
-- NOTE: This is the only operator which cannot be expressly typed
-- "...A$~[!..." for instance will effectively add the operator midline
del :: ProgramState -> ProgramState
del = hand form3 (\x -> 0) (\x -> "")


-- Get string function
-- Operator: ` `
-- Having IO issues with this
getString :: ProgramState -> ProgramState
getString state = do
    (prog,pt,ext,r) <- state
    inp <- getLine
    let (xs,x,ys) = gobble prog
    let newprog = xs ++ inp ++ ys
    let npt = length (xs ++ inp)
    return (newprog,npt,ext,r)

-- Subroutine function calls intr below
-- Operator: `;`
subr :: ProgramState -> ProgramState
subr state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    res <- runStringy $ return (xs,0,ext,True)
    let newprog = res ++ ys
    let npt = length res
    return (newprog,npt,ext,r)


-- Jump to function
-- Operator: `@`
jump :: ProgramState -> ProgramState
jump state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = xs ++ (tail ys)
    let npt = ord $ head ys
    return (newprog,npt,ext,r)


-- Equal function
-- Operator: `=`
eq :: ProgramState -> ProgramState
eq state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = if xs == "" then "0"++(tail ys) else (init xs)++(if (last xs) == (head ys) then "1" else "0")++(tail ys)
    return (newprog,pt,ext,r)

-- length of previous string
-- Operator: `#`
length' :: ProgramState -> ProgramState
length' = hand form3 fwr (\x -> x++[chr . (`mod` 128) . length $ x])

-- Keeps the preceding string as a Subroutine
-- Operator: `:`
keep :: ProgramState -> ProgramState
keep state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    return (ys,0,xs,r)

-- call the kept function
-- Operator: `|`
call :: ProgramState -> ProgramState
call state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let newprog = xs ++ ext ++ ys
    let npt = pt + (length ext)
    return (newprog,npt,ext,r)


-- operates the function on only a single char instead of the whole string
-- Operator: `*`
op1 :: ProgramState -> ProgramState
op1 state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    (res,pti,_,_) <- interpret $ return ([last xs]++ys,1,ext,r)
    let newprog = (init xs)++res
    let npt = pt + pti - 1
    return (newprog,npt,ext,r)

-- Uses up the current char but changes nothing otherwise
use :: ProgramState -> ProgramState
use = hand form1 fwr id

-- Uppercase 
-- Operator: `"`
upper :: ProgramState -> ProgramState
upper = hand3 (map toUpper)

-- Lowercase
-- Operator: `'`
lower :: ProgramState -> ProgramState
lower = hand3 (map toLower)

-- Return to 0
-- Operator: `,`
ret :: ProgramState -> ProgramState
ret = hand form3 (\x -> 0) id

-- Jump backward over previous operator
-- Opeartor: `!`
back_skip :: ProgramState -> ProgramState
back_skip = hand form3 bck id

-- Print current value
-- Operator: ```
out :: ProgramState -> ProgramState
out state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble pt prog
    putStrLn xs
    return (ys,0,ext,True)

--Replace
-- Operator: `&`
repl :: ProgramState -> ProgramState
repl state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
    let t = last xs
    let h = head ys
    let nxs = map (\u -> if u == t then h else u) xs
    let newprog = nxs++(tail ys)
    return (newprog,pt,ext,r)

-- Terminate function (and print)
-- Operator: `.`
end :: ProgramState -> ProgramState
end state = do
    (prog,pt,ext,r) <- state
    let (xs,x,ys) = gobble prog
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
    | x == '&' = id' -- not implemented yet
    | x == '\'' = lower  
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

-- The function which actually steps 
-- between ProgramStates
-- Also corrects for pointer issues
interpret :: ProgramState -> ProgramState
interpret state = do
    (prog,pt,ext,r) <- state
    let npt = pt `mod` (length prog)
    let op = prog !! npt
    (getOp op) $ return (prog,npt,ext,r)

-- Handles the recursive calling of `interpret`
-- Returns the Program 
runStringy :: ProgramState -> IO Program
runStringy state = do
    (prog,pt,ext,r) <- state
    if r then
        runStringy $ interpret state
    else
        return prog

-- For debugging runs through each `n` states
debug :: Int -> ProgramState -> IO ()
debug 0 state = do
    s <- state
    putStrLn $ show s
debug n state = do
    s <- state
    putStrLn $ show s
    debug (n - 1) (interpret state)

-- Inital program state for commandline entry
initialState :: ProgramState
initialState = return (" ",0,"",True)

-- Run a string command from command line
run :: IO ()
run = do
    putStrLn " -- Stringy Command Line -- "
    putStrLn " Enter Program: "
    putStr ">>"
    output <- runStringy initialState
    putStrLn output
