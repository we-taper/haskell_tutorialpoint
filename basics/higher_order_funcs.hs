-- A good show case of using higher order functions
divideTen :: (Floating a) => a -> a
divideTen = (/10)
-- However, if 10 is a function, this won't work...
{- The following won't pass the compilation:
plusOne :: (Floating a) => a -> a
plusOne x = x + 1
testF :: (Floating a) => a -> a -> a
testF = (/plusOne)
-}
-- Using higher order functions in a mixed sense
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  
-- This is a particularly good example
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
-- This is also interesting
listOfFuncs = map (*) [0..]  -- This creates a list of functions

-- Lambda functions
chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)
numLongChains = length (filter (\xs -> length xs > 15) (map chain [1..100]))


-- Only folds and horses
-- Functions which accumulates something over a list
sumall :: (Num a) => [a] -> a
sumall = foldl (+) 0
---- Demonstrate the difference between foldl and foldr
sumallL = foldl (++) "A" ["a", "b", "c"]
sumallR = foldr (++) "A" ["a", "b", "c"]


-- Point free style
pointFreeFn = ceiling. negate . tan . cos . max 50
{-Note that the  following would not make sense due to the parentheses
pointFreeFn = ... cos(max 50)
-}

-- The $ has the lowest priority
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]  
-- And it tells us that function application can be treated just like another function
alist = map ($ 3) [(4+), (10*), (^2), sqrt]

main = do
    print(divideTen 10)
    print(applyTwice (++ "hello") "2 hellos: ")
    print(zipWith (+) [3,1,3,1] [5,3,1,10])
    print((listOfFuncs !! 4) 5 )
    print(numLongChains)
    print(sumall [1,2,3,4,5])
    print(sumallL)
    print(sumallR)
    print(alist)