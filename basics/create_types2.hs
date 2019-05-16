-- Recursive data types example: Binary max-heap
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
singleton :: a -> Tree a  
singleton x = Node x EmptyTree EmptyTree  
  
treeInsert :: (Ord a) => a -> Tree a -> Tree a  
treeInsert x EmptyTree = singleton x  
treeInsert x (Node a left right)   
    | x == a = Node x left right  
    | x < a  = Node a (treeInsert x left) right  
    | x > a  = Node a left (treeInsert x right)  
treeElem :: (Ord a) => a -> Tree a -> Bool  
treeElem x EmptyTree = False  
treeElem x (Node a left right)  
    | x == a = True  
    | x < a  = treeElem x left  
    | x > a  = treeElem x right

-- TYPECLASS and INSTANCES
-- Summary: class is for defining new typeclasses and instance is for making our types instances of typeclasses.
-- Class
class Eq' a where  
    eqq :: a -> a -> Bool  
    neqq :: a -> a -> Bool  
    x `eqq` y = not (x `neqq` y)  
    x `neqq` y = not (x `eqq` y)  
-- Instance
data TrafficLight = Red | Yellow | Green  
instance Eq' TrafficLight where  
    Red `eqq` Red = True  
    Green `eqq` Green = True  
    Yellow `eqq` Yellow = True  
    _ `eqq` _ = False  -- a catch-all pattern saying that if it's none of the previous combinations, then two lights aren't equal.
-- Another instance example
instance Show TrafficLight where  
    show Red = "Red light"  
    show Yellow = "Yellow light"  
    show Green = "Green light"  

-- One has to be careful when using abstract things like Maybe in class declarations
-- Because Maybe is not a concrete type, it is a type constructor.
-- For example: below does not work
{-
instance Eq' (Maybe m) where  
    Just x `eqq` Just y = x `eqq` y  
    Nothing `eqq` Nothing = True  
    _ `eqq` _ = False  
-}
-- Whereas, below works:
instance (Eq' m) => Eq' (Maybe m) where  
    Just x `eqq` Just y = x `eqq` y  
    Nothing `eqq` Nothing = True  
    _ `eqq` _ = False

main = do
    let nums = [8,6,4,1,7,3,5]
    let numsTree = foldr treeInsert EmptyTree nums
    print(numsTree)
    print(Red `eqq` Red)
    print(Yellow `neqq` Red)
    print(Yellow `neqq` Red)
    print(Red)
    print(Yellow)