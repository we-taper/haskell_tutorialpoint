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
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavour :: String  
                     } deriving (Show)
-- which automatically creates the look-up functions such as: firstName


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
    let guy = Person "Buddy" "Finance" 43 184.2 "526-2928" "Chocolate"
    print(firstName guy)
    print(flavour guy)
    print(guy)

    print(lockerLookup 101 lockers)
    print(lockerLookup 100 lockers)
    print(lockerLookup 102 lockers)
    print(lockerLookup 110 lockers)
    print(lockerLookup 105 lockers)