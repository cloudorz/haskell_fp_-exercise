import Test.QuickCheck

half x = x / 2

halfIdentity = (*2) . half

main :: IO ()
main = quickCheck (\x -> halfIdentity x == (x :: Float))
