
module Main where

import Test.Tasty

import Test.Data.ShiftList

main :: IO ()
main = defaultMain $ testGroup "All tests" 
    [ shiftListTests ]
