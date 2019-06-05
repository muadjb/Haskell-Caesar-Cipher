module Cipher (shiftRight,shiftLeft, caesar, unCaesar) where

import Data.Char

lowerBound = ord 'a'
upperBound = ord 'z' + 1
range = upperBound - lowerBound

shiftRight :: Int -> Char -> Char
shiftRight n c 
  | x >= upperBound = shiftRight (n-range) c
  | otherwise = chr x
  where x = (ord c + n)

shiftLeft :: Int -> Char -> Char
shiftLeft n c 
  -- | x < lowerBound = shiftLeft (n+range) c
  | x < lowerBound = shiftLeft (n-1) 'z'
  | otherwise = chr x
  where x = (ord c - n)

-- shift n c 
--   | x >= upperBound = chr (mod x upperBound + lowerBound) 
--   | otherwise = chr x
--   where x = (n + ord c)

caesar :: Int -> String -> String
caesar shiftCount source = 
  fmap (shiftRight shiftCount) source

unCaesar :: Int -> String -> String
unCaesar shiftCount source = 
  fmap (shiftLeft shiftCount) source