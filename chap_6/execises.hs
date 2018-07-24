-- 2.1
data Person = Person Bool

instance Show Person where
  show (Person b) = "Person: " ++ (show b)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.2
data Mood = Blah
         | Woot deriving Show

settleDown x = if x == Woot 
                then Blah
                else x

instance Eq Mood where
  Blah == Blah = True
  Woot == Woot = True
  _ == _ = False

--data Example = MakeExample deriving Show
data Example = MakeExample Int deriving Show
