module SRS.Rule.Type
( Rule (..)
, lhs
, rhs
) where

data Rule t = Rule t t | DPRule t t 
    deriving (Eq, Ord, Show)

lhs (Rule l _) = l
lhs (DPRule l _) = l

rhs (Rule _ r) = r
rhs (DPRule _ r) = r
