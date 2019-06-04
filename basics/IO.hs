import Data.Char  
  
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?" 
    {- Notice how we used $ here to avoid too many parentheses.
       More explanation here: 
       https://stackoverflow.com/questions/940382/what-is-the-difference-between-dot-and-dollar-sign
    -}