import Stringy

-- simple command line Stringy interpreter

main = do
    program <- getLine
    putStrLn $ intr 0 program
    
