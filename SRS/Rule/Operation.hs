module SRS.Rule.Operation
( isLengthPreserving
, uniqSymbolList
) where

import SRS.Rule.Type
import SRS.Term

import Data.Monoid 
import Data.Set (Set, toList, fromList)

{-
    A rule is length preserving is both sides have the same symbol count
-}

isLengthPreserving :: SRSTerm t => Rule (t f) -> Bool
isLengthPreserving rule = (symbolCount . lhs) rule == (symbolCount . rhs) rule 

uniqSymbolList :: (SRSTerm t, Monoid (t a), Ord a) => Rule (t a) -> [a]
uniqSymbolList r = (toList . fromList . asList) $ append (lhs r) (rhs r)
