
{-# LANGUAGE DeriveTraversable #-}

module Data.ShiftList 
    ( ShiftList
    , shiftLeft
    , shiftLeft'
    , shiftRight
    , shiftRight'
    , fromList
    , fromNonEmpty
    , lookup
    , Data.ShiftList.length ) where

import Control.Comonad
import Data.Foldable
import Data.Sequence (Seq, ViewL(..), ViewR(..), viewl, viewr, (|>), (<|))
import qualified Data.Sequence as S
import Data.List.NonEmpty (NonEmpty(..))
import Prelude hiding (lookup, length)

data ShiftList a 
    = ShiftList 
    { slLeft  :: Seq a
    , slVal   :: a 
    , slRight :: Seq a  }
    deriving (Show, Eq, Functor, Foldable, Traversable)

instance Applicative ShiftList where
    pure x = ShiftList S.empty x S.empty
    ShiftList fls f frs <*> ShiftList xls x xrs
        = ShiftList (fls <*> xls) (f x) (frs <*> xrs)

instance Comonad ShiftList where
    extract (ShiftList _ x _) = x
    duplicate = fmap pure

-- | Shift the current position to the left, returning True if wrapping
--   around was necessary.
shiftLeft :: ShiftList a -> (Bool, ShiftList a)
shiftLeft sl@(ShiftList l x r) =
    case (viewr l, viewr r) of
        (EmptyR, EmptyR) -> (True, sl)
        (EmptyR, s :> y) -> (True, ShiftList (x <| s) y S.empty)
        (s :> y, _     ) -> (False, ShiftList s y (x <| r))

shiftLeft' :: ShiftList a -> ShiftList a
shiftLeft' = snd . shiftLeft

-- | Shift the current position to the right, returning True if wrapping
--   around was necessary.
shiftRight :: ShiftList a -> (Bool, ShiftList a)
shiftRight sl@(ShiftList l x r) =
    case (viewl l, viewl r) of
        (EmptyL, EmptyL) -> (True, sl)
        (y :< s, EmptyL) -> (True, ShiftList S.empty y (s |> x))
        (_     , y :< s) -> (False, ShiftList (l |> x) y s)

shiftRight' :: ShiftList a -> ShiftList a
shiftRight' = snd . shiftRight

fromList :: [a] -> Maybe (ShiftList a)
fromList [] = Nothing
fromList (x:xs) = Just $ ShiftList S.empty x (S.fromList xs)

fromNonEmpty :: NonEmpty a -> ShiftList a
fromNonEmpty (x :| xs) = ShiftList S.empty x (S.fromList xs)

lookup :: Int -> ShiftList a -> Maybe a
lookup i (ShiftList l x r)
    | ll >  i    = S.lookup i l
    | ll == i    = Just x
    | otherwise  = S.lookup (i - 1 - ll) r
  where
    ll = S.length l

length :: ShiftList a -> Int
length (ShiftList l _ r) = S.length l + 1 + S.length r
