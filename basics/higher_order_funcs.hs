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

main = do
    print(divideTen 10)
    print(applyTwice (++ "hello") "2 hellos: ")
    print(zipWith (+) [3,1,3,1] [5,3,1,10])