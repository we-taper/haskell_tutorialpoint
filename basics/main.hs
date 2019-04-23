{-
chaining_multiple_io_wont_work = 
    print("asdf")
    print("asdf2")
-}
single_io_works = 
    print("asdf")

chaining_multiple_io_works_only_with_do = do
    let var1 = 2
    let var2 = 3
    {-
    The following syntax is only allowed in GHCi:
    var3 = 4
    var4 = 5
    -}
    putStrLn "asdfasdf:"
    print(var1 + var2)

    -- List comprehension
    let alist = [x ^ 2 | x <- [1..10]]
    print(alist)
-- Compiler complains if a main does not exist
main = chaining_multiple_io_works_only_with_do 