import Data.Char (toUpper, isSpace)

capitalizeWord :: String -> String 
capitalizeWord [] = []
capitalizeWord (c : cs) = toUpper c : cs

capitalizeParagraph :: String -> String
capitalizeParagraph = concat . fmap capitalizeWord . sentences
  where
    sentences [] = []
    sentences p = case break (== '.') p of 
                       (l, (p:s)) -> (l ++ [p] ++ takeWhile isSpace s) : (sentences (dropWhile isSpace s))

