lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   
unlucky :: (Integral a) => a -> String
unlucky x = "Unluckily, the other return values are ignored."
unlucky 7 = "I am a secret." -- This would give the following warning:
{-
syntaxInFunc.hs:6:1: warning: [-Woverlapping-patterns]
    Pattern match is redundant
    In an equation for ‘unlucky’: unlucky 7 = ...
  |
6 | unlucky 7 = "I am a secret."
  | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-}
-- Pattern matching can be used to define recursive functions:
factoral :: (Integral a) => a -> a
factoral 0 = 1
factoral n = n * factoral (n-1)
-- Interestingly, in ghci we cannot do the type declaration three lines above.
-- And in that case, a factoral function defined as above hangs the program.
--------------------------------------------
-- Strange things used inside pattern matching:
-- tuples
first :: (a, b, c) -> a
first (x, _, _) = x
-- list
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x 
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell ([x]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

main = do
    print(lucky 3)
    print(lucky 7)
    print(unlucky 3)
    print(unlucky 7)
    print(factoral 10)
    {-The following would not even pass the compilation check:
    print(factoral -1)
    -}
    print(first (1, 2, 3))
    print(head' "asdf")
    {- The following raises an exception:
    *** Exception: Can't call head on an empty list, dummy!
CallStack (from HasCallStack):
  error, called at syntaxInFunc.hs:28:12 in main:Main

    print(head' "")
    -}
    print(tell "")
    print(tell [1])
    print(tell "ab")
    print(tell "asdf")
    {- Interestingly, this won't compile:
    print(tell [])
    Maybe it is due to (Show a) in the definition?
    -}