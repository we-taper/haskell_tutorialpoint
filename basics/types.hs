import Data.Typeable
-- Here is what a type declaration looks like:
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z
addThree' x y z = x + y + z

main = do
    print(typeOf addThree)
    {- However, this won't work: print(typeOf addThree') -}
    -- There is also an interestingly named type: less than (LT), equal (EQ), greater than (GT).
    print([LT .. GT]) -- > [LT,EQ,GT]
    print(read "5" + 4) -- This works, but
    {- This does not work:
    print(read "5")
    -}
    print(read "5" :: Int) -- If haskell cannot guess the type, we need to tell it.
    print(typeOf (length [1,2,3])) -- It is not a number
    print(fromIntegral(length [1,2,3]) + 3.1) -- So have to use fromIntegral to convert it to a general number class.
    {-
    I can only show the types of variable types inside ghci. Try the following commands inside ghci:
    :t head
    :t (+)
    -}