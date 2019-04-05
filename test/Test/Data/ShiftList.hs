

module Test.Data.ShiftList (shiftListTests) where

import qualified Data.Sequence as S

import Control.Comonad
import Data.Maybe
import Test.Tasty
import Test.Tasty.HUnit

import Data.ShiftList
import Prelude hiding (lookup)

nothingSL, oneSL, threeSL :: Maybe (ShiftList Int)
nothingSL = fromList []
oneSL     = fromList [1]
threeSL   = fromList [1..3]

shiftListTests :: TestTree
shiftListTests = testCaseSteps "ShiftList" $ \_ -> do
    nothingSL @?= Nothing

--    oneSL @?= Just (ShiftList S.empty 1 S.empty)
    oneSL @?= Just (pure 1)
    extract <$> oneSL @?= Just 1

    let leftOne = shiftLeft . fromJust $ oneSL
    leftOne @?= (True, pure 1)
    (extract . snd $ leftOne) @?= 1

    let rightOne = shiftRight . fromJust $ oneSL
    rightOne @?= (True, pure 1)
    (extract . snd $ rightOne) @?= 1

--    threeSL @?= Just (ShiftList S.empty 1 (S.fromList [2,3]))
    extract <$> threeSL @?= Just 1
    
    let leftThree = shiftLeft . fromJust $ threeSL
--    leftThree
--       @?= (True, ShiftList (S.fromList [1,2]) 3 S.empty)
    (extract . snd $ leftThree) @?= 3
    lookup <$> [0..2] <*> [snd leftThree] @?= Just <$> [1..3]
    lookup (-1) (snd leftThree) @?= Nothing
    lookup 3 (snd leftThree) @?= Nothing


    let rightThree = shiftRight . fromJust $ threeSL
--    rightThree
--        @?= (False, ShiftList (S.fromList [1]) 2 (S.fromList [3])) 
    (extract . snd $ rightThree) @?= 2
    lookup <$> [0..2] <*> [snd rightThree] @?= Just <$> [1..3]
    lookup (-1) (snd rightThree) @?= Nothing
    lookup 3 (snd rightThree) @?= Nothing
