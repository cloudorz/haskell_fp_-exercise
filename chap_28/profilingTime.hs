-- profilingTime.hs
module Main where

f :: IO () 
f = do
  print ([1..] !! 999999) 
  putStrLn "f"

g :: IO () 
g = do
  print ([1..] !! 9999999) 
  putStrLn "g"

main :: IO () 
main = do
  f
  g

 --stack ghc -- -prof -fprof-auto -rtsopts -O2 profile.hs
 -- ./profile +RTS -P
 -- 
