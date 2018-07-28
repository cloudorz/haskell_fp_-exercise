import Data.Char (ord, chr, isSpace)
import Data.List

type Key = String

encode :: Key -> String -> String
encode key ps = encodeOrDecode key ps f
  where 
    baseOrd = ord 'A'
    f a b = chr $ baseOrd + mod (ord a + ord b - 2 * baseOrd) 26
  

decode :: Key -> String -> String
decode key ps = encodeOrDecode key ps f
  where 
    baseOrd = ord 'A'
    f a b = chr $ baseOrd + mod (26 + ord a - ord b) 26

encodeOrDecode :: Key -> String -> (Char -> Char -> Char) -> String
encodeOrDecode key ps convertLetter = convert ps 0
  where 
    convert [] _ = []
    convert (c:cs) count = if isSpace c 
                           then c : (convert cs count)
                           else convertLetter c (oneKeyByIndex count) : (convert cs (count + 1))
    oneKeyByIndex i = key !! mod i (length key)
