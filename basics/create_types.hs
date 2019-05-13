import qualified Data.Map as M  

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


-- ignored many different ways to export and import things

-- Note worthy is: Record Syntax
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     } deriving (Show, Eq, Ord)
-- which automatically creates the look-up functions such as: firstName, lastName, etc.
-- deriving from Show and Eq automatically creates some simple and convenient functions for Show and `==` operators.
-- In fact, it is quite common that when a type derives something, a default implementation is already in place.

-- Creates a type with type parameters
data Vector a = Vector a a a deriving (Show)  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
scalarMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `scalarMult` m = Vector (i*m) (j*m) (k*m)  
vectMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `vectMult` (Vector l m n) = i*l + j*m + k*n  



-- type synonyms
phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("penny","853-2492")     
    ]
showPhoneBook :: [(String, String)] -> String
showPhoneBook book = foldl1 (++) [show a | a <- book]
-- type synonyms does nothing, but it improves the code
type PhoneBookType = [(String, String)]
showPhoneBookType :: PhoneBookType -> String
showPhoneBookType book = foldl1 (++) [show a | a <- book]
-- parametrised type synonyms with partially filled type parameters
type IntMap v = M.Map Int v  -- which is equivalent to `type IntMap = Map Int`

-- Another very interesting "or" type synonyms, which is used here to indicate
-- some kind of signals.
data LockerState = Taken | Free deriving (Show, Eq)  
type Code = String  
type LockerMap = M.Map Int (LockerState, Code)
lockerLookup :: Int -> LockerMap -> Either String Code  
lockerLookup lockerNumber map =   
    case M.lookup lockerNumber map of   
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"  
        Just (state, code) -> if state /= Taken   
                                then Right code  
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
lockers :: LockerMap  
lockers = M.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]

main = do
    print(Circle 10 10 3) -- we can do this because Shape derives Show
    print(map (Circle 10 20) [4,5,6,6]) -- Circuit is in fact a function which could be mapped.
    let guy = Person "Buddy" "Finance" 43
    print(firstName guy)
    print(guy)

    let mikeD = Person {firstName = "Michael", lastName = "Diamond", age = 43}
    let adRock = Person {firstName = "Adam", lastName = "Horovitz", age = 41}
    print(mikeD == adRock)
    print(mikeD == mikeD)
    print(mikeD == Person {firstName = "Michael", lastName = "Diamond", age = 43})

    -- Even elem works now
    print(mikeD `elem` [mikeD, adRock])
    -- We could even compare types:
    print(Person "a" "b" 1 < Person "a" "c" 1)
    print(Person "a" "b" 1 > Person "a" "b" 0)

    print(Vector 1 2 3)
    print(Vector 1 2 3 `scalarMult` 10)
    print(Vector 1 1 1 `vectMult` Vector 1 (-1) 1)

    print(lockerLookup 101 lockers)
    print(lockerLookup 100 lockers)
    print(lockerLookup 102 lockers)
    print(lockerLookup 110 lockers)
    print(lockerLookup 105 lockers)