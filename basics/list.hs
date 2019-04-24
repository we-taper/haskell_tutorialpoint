main = do
{-
Something unexpected from list
-}
-- 1. It must be homogeneous
--    [1,2,"3"] does not work
--    But [[1],[2],[]] works...
-- 2. It is 0-based, accessed via !! operator
    print([1,2,3] !! 1 == 2)  -- True
-- 3. It can be concatenated via ++ operator
    print([1,2,3] ++ [4])
    print("asdf" ++ "hjkl")
-- 4. It is fast to add to prepend with :, slow to append with ++. Actually
    print(1:2:3:[] == [1,2,3])  -- True
-- A lot of list function exists. The strangest is `is inside` function:
    print(4 `elem` [3,4,5,6])
-- Float list with steps works but is dangenrous:
    print([0.1, 0.3 .. 1]) -- [0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]
-- Because Haskell is lazy, we can do
    print(take 10 [1,2..]) -- [1,2,3,4,5,6,7,8,9,10]
-- list comprehension is beautiful
    print([ x | x <- [1..20], odd x, x < 10])  -- [1,3,5,7,9]