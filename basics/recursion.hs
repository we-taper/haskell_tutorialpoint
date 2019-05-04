{- A demo of quicksort implementation in Haskell
However, this is not an efficient quicksort. It does not operation on the
array in an in-place fashion. And people talks at length about this here:
https://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort
-}
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted

main = do
    print(quicksort [81,1,2,3,8,5])