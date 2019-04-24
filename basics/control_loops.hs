doubleSmallPositiveNumber x = 
    if x > 100 || x < 0
    then x
    else x*2
main = do
    print(doubleSmallPositiveNumber 3)
    print(doubleSmallPositiveNumber (-3))
    {-
    There is a difference between above and below
    print(doubleSmallPositiveNumber -3)
    which gives the error stack message:
    • No instance for (Num (a0 -> a0)) arising from a use of ‘-’
        (maybe you haven't applied a function to enough arguments?)
    • In the first argument of ‘print’, namely
        ‘(doubleSmallPositiveNumber - 3)’
      In a stmt of a 'do' block: print (doubleSmallPositiveNumber - 3)
      In the expression:
        do print (doubleSmallPositiveNumber 3)
           print (doubleSmallPositiveNumber - 3)
    -}