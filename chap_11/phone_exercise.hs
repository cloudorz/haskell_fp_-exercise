import Data.Char (isUpper, toUpper, isSpace)
import Data.List
import Data.Maybe

-- validButtons = "1234567890*#"
type Digit = Char
-- Valid presses: 1 and up
type Presses = Int

data DaPhone = DaPhone [(Digit, [Char])]

defaultDaPhone :: DaPhone
defaultDaPhone = DaPhone [('1', "1")
                        , ('2', "ABC2")
                        , ('3', "DEF3")
                        , ('4', "GHI4")
                        , ('5', "JKL5")
                        , ('6', "MNO6")
                        , ('7', "PARS7")
                        , ('8', "TUV8")
                        , ('9', "WXYZ9")
                        , ('*', "^*")
                        , ('0', "+_0")
                        , ('#', ".,#")]

convo :: [String] 
convo =
       ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol lol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Haha thanks just making sure rofl ur turn"]

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone@(DaPhone keys) c = maybeToList ((fmap . fmap) (count bigC) pair) ++ (if isUpper c then reverseTaps phone '^' else [])
  where 
    bigC = if isSpace c then '+' else toUpper c
    pair = find (\(d, cs) -> elem bigC cs) keys 
    count c cs = case elemIndex c cs of 
                      Just index -> index + 1
                      Nothing -> -1

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)] 
cellPhonesDead phone cs = cs >>= (reverseTaps phone)  

fingerTaps :: [(Digit, Presses)] -> Presses 
fingerTaps = sum . fmap snd

letterFreq :: Eq a => [a] -> [(a, Int)]
letterFreq [] = []
letterFreq cs@(c:_) = let allSameLetters = intersect cs [c] 
                      in (c, length allSameLetters) : letterFreq (cs \\ allSameLetters) 

maximumFreqTuple :: Eq a => [a] -> (a, Int)
maximumFreqTuple = maximumBy compareTuple . letterFreq
  where 
    compareTuple (_, a) (_, b) = compare a b

mostPopularLetter :: String -> Char 
mostPopularLetter = fst . maximumFreqTuple

mostPopularLetterCost :: (Char, Int) -> Int
mostPopularLetterCost (c, freq) = fingerTaps (reverseTaps defaultDaPhone c) * freq

coolestLtr :: [String] -> Char 
coolestLtr = mostPopularLetter . concat

coolestWord :: [String] -> String 
coolestWord = fst . maximumFreqTuple . concat . fmap words

