import Data.Char
import Data.Bits

sp f x = map (chr . (`mod` 128) . f . ord) x --this form is used multiple times where you want to map over a string, take the ordinal of the char then use f, mod 128, back to chr
sL x = shiftL x 1 --short hand for left shift by 1
sR x = shiftR x 1 --short hand for right shift by 
shuff' "" = ""
shuff' x 
    |odd n = (shuff' ((map (x!!) [0..((quot n 2)-1)])++(map (x!!) [((quot n 2)+1)..(n-1)])))++((x!!(quot n 2)):[]) 
    |otherwise = ((x!!0):(x!!(quot n 2)):[])++(shuff' ((map (x!!) [1..((quot n 2)-1)])++(map (x!!) [((quot n 2)+1)..(n-1)]))) 
    where n = length x
rep' x n = foldl (++) "" (replicate n x)


id' xs x ys = xs++(x:[])++ys --the identity function for the form String -> Char -> String -> String
inc xs x ys = (sp succ xs)++ys -- increments xs 
dec xs x ys = (sp pred xs)++ys -- decrements xs
sXL xs x ys = (sp sL xs)++ys -- shifts left xs
sXR xs x ys = (sp sR xs)++ys -- shifts right xs
times n xs x ys = (sp ((+n) . (*10)) xs)++ys -- multiples xs by 10 plus n
rep xs x ys = (rep' xs (ord (head ys)))++(tail ys)
piv xs x ys = (init xs)++((head ys):[])++((last xs):[])++(tail ys)
readUs xs x ys = id' xs x ys -- this may not be possible
exor xs x ys = id' xs x ys --(sp (xor (head ys)) xs)++(tail ys)
front xs x ys = ((head ys):[])++xs++(tail ys) -- moves the head of ys to the front of the string
back xs x ys = xs++(tail ys)++((head ys):[]) -- moves the head of ys to the back of the string
end xs x ys = xs++ys
mod' xs x ys = (sp (`mod` (ord (head ys))) xs)++(tail ys)
plus xs x ys = (sp ((+) (ord (head ys))) xs)++(tail ys)
divi xs x ys = (sp (`quot` (ord (head ys))) xs)++(tail ys)
mult xs x ys = (sp (* (ord (head ys))) xs)++(tail ys)
minus xs x ys = (sp (subtract (ord (head ys))) xs)++(tail ys)
shuff xs x ys = (shuff' xs)++ys
grteq xs x ys = (init xs)++((if (ord (last xs)) >= (ord (head ys)) then (chr 1) else (chr 0)):[])++(tail ys)
rev xs x ys = (reverse xs)++ys


comlist = [id',id',id',id',id',mod',id',id',front,back,mult,plus,readUs,minus,end,divi,(times 0),(times 1),(times 2),(times 3),(times 4),(times 5),(times 6),(times 7),(times 8),(times 9),id',id',sXL,id',sXR,shuff,id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',inc,grteq,dec,piv,rep,id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',id',rev,id']

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
