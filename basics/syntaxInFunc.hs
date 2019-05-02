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
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)
-- Interestingly, in ghci we cannot do the type declaration three lines above.
-- And in that case, a factorial function defined as above hangs the program.
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

-- Language sugar
-- a_sugar :: (Show a) => [a] -> String
a_sugar :: [Char] -> String
a_sugar all@(a1:a2:rest) = "The list " ++ all ++ " is [" ++ [a1] ++ ", " ++ [a2] ++ ", " ++ rest ++ "]"

-- An alternative to multiple f-else
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat = "You're fat! Lose some weight, fatty!"  
    | otherwise = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2  
          (skinny, normal, fat) = (18.5, 25.0, 30.0) -- The indentation in the front is important!

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi w h | (w, h) <- xs]
  where bmi w h = w / h ^ 2

-- Use function in the infix way
leq a b = a <= b  -- see usage in main

-- Case expression is pretty cool
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  

{- An equivalent def
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."
-}
main = do
    print(lucky 3)
    print(lucky 7)
    print(unlucky 3)
    print(unlucky 7)
    print(factorial 10)
    {-The following would not even pass the compilation check:
    print(factorial -1)
    -}
    print(first (1, 2, 3))
    print(head' "abcde")
    {- The following raises an exception:
    *** Exception: Can't call head on an empty list, dummy!
CallStack (from HasCallStack):
  error, called at syntaxInFunc.hs:28:12 in main:Main

    print(head' "")
    -}
    print(tell "")
    print(tell [1])
    print(tell "ab")
    print(tell "abcde")
    {- Interestingly, this won't compile:
    print(tell [])
    Maybe it is due to (Show a) in the definition?
    -}
    print(a_sugar "abcdefg")
    print(bmiTell 60 170)
    print(leq 1 3)
    print(1 `leq` 3)
    print(calcBmis [(50, 170), (60, 175), (70, 175)])

    -- let makes an expressions, and therefore can be used as:
    print(4 * (let a = 9 in a + 1) + 2)  -- 42
    -- It can also introduces multiple score
    print(let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  -- (6000000,"Hey there!")
    print(describeList [])
    print(describeList [1,2])
    print(describeList [2])