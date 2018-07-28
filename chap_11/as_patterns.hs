import Data.List
import Data.String (words)

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf ts xs = and $ flip elem xs <$> ts

capitalizeWords :: String -> [(String, String)]
capitalizeWords = fmap wordPair . words
  where 
    wordPair cs@(c:rest) = (cs, toUpper c : rest)

