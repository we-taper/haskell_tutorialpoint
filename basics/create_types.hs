data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


-- ignored many different ways to export and import things

-- Note worthy is: Record Syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavour :: String  
                     } deriving (Show)
-- which automatically creates the look-up functions such as: firstName

main = do
    print(Circle 10 10 3) -- we can do this because Shape derives Show
    print(map (Circle 10 20) [4,5,6,6]) -- Circuit is in fact a function which could be mapped.
    let guy = Person "Buddy" "Finance" 43 184.2 "526-2928" "Chocolate"
    print(firstName guy)
    print(flavour guy)
    print(guy)