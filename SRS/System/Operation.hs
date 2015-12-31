module SRS.System.Operation
( trivialNonTerm
, lengthPreserving
, getSignature
, toDPSystem
, ruleCount
, srsRules
, dpRules
, catRules
, translateSRS
, translateRule
, translateWord
, isDPSystem
) where

import SRS.Term
import SRS.System.Type
import SRS.Rule.Type
import SRS.Rule.Operation

import Data.Monoid
import Data.Map as M
import Data.Set as S
import Data.List as L

{-
    A system is trivially nonterminating if there exists a rule with
        * lhs = rhs
        * lhs is empty

-}

trivialNonTerm :: (SRSTerm t , Eq (t f), Eq f) => System (t f) -> Bool
trivialNonTerm sys = or $ do
    rule <- catRules sys
    if (lhs rule == rhs rule || isEmpty (lhs rule))
        then return True
        else return False

{-
    A system is length preserving iff every rule is length preserving
-}

--lengthPreserving :: SRSTerm t => System t f -> Bool
lengthPreserving :: SRSTerm t => System (t f) -> Bool
lengthPreserving sys = case sys of
        SRS rs           -> lp rs
        DPProblem rs rrs -> lp rrs && lp rs
    where lp = L.foldr ((&&) . isLengthPreserving) True

{-
    Get the signature of a system
-}
getSignature :: (SRSTerm t, Monoid (t a), Ord a) => System (t a) -> Set a
getSignature (SRS l) = 
    L.foldr (S.union . S.fromList . uniqSymbolList) S.empty l

getSignature (DPProblem l _) = 
    L.foldr (S.union . S.fromList . uniqSymbolList) S.empty l


isDPSystem (DPProblem _ (d:ds)) = True
isDPSystem _ = False

{-
    Utility for systems
-}
ruleCount :: System t -> Int
ruleCount (SRS r) = length r
ruleCount (DPProblem r d) = length r + length d

--srsRules :: System t s -> [Rule (t s)]
srsRules :: System t -> [Rule t]
srsRules (SRS r) = r
srsRules (DPProblem r _) = r

--dpRules :: System t s -> Maybe [Rule (t s)]
dpRules :: System t -> Maybe [Rule t]
dpRules (DPProblem _ r) = Just r
dpRules _ = Nothing

--catRules :: System t s -> [Rule (t s)]
catRules :: System t -> [Rule t]
catRules (SRS r) = r
catRules (DPProblem r d) = d ++ r

{-
    DP-transformation
-}
toDPSystem :: (SRSTerm t, Monoid (t f), Ord (t f), Ord f) 
    => System (t f) -> System (t f)
toDPSystem (SRS rules) = case (getDPRules (getDPSymbols rules) rules) of
    []      ->  SRS rules 
    (r:rs)  ->  DPProblem rules (r:rs)
toDPSystem (DPProblem rules _) = case (getDPRules (getDPSymbols rules) rules) of
    []      ->  SRS rules 
    (r:rs)  ->  DPProblem rules (r:rs)

getDPRules :: (SRSTerm t, Monoid (t f), Ord (t f), Ord f) 
    => Set f -> [Rule (t f)] -> [Rule (t f)]
getDPRules dpSymbols rules = S.toList 
    $ L.foldr (S.union) S.empty 
    $ fmap (getDPs dpSymbols) rules

getDPs :: (SRSTerm t, Monoid (t f), Ord (t f), Ord f) 
    => Set f -> Rule (t f) -> Set (Rule (t f))
getDPs dpSymbols (Rule l r) = S.fromList . asList $
    fmap (DPRule l) $
    SRS.Term.filter (isDP dpSymbols l) $ subterms r

isDP :: (SRSTerm t, Monoid (t f), Ord f, Eq (t f)) 
    => Set f -> t f -> t f -> Bool
isDP dpSymbols l r = case root r of
    Just fr -> (S.member fr dpSymbols) && (not (isProperSubterm l r))
    Nothing -> False 

-- set of root symbols from left sides
getDPSymbols :: (SRSTerm t, Ord a) => [Rule (t a)] -> Set a
getDPSymbols sr = L.foldr (addRoot . lhs) S.empty sr

-- add a root symbol to a set if it exists
addRoot :: (SRSTerm t, Ord a) => t a -> Set a -> Set a
addRoot l set = case root l of
    Just s -> S.insert s set
    _      -> set

{- For translating symbols by a map
-}
translateSRS
  :: (Ord a, Functor f) => Map a b -> System (f a) -> System (f b)
translateSRS m (SRS r) = SRS $ fmap (translateRule m) r
translateSRS m (DPProblem r d) = DPProblem (fmap (translateRule m) r) (fmap (translateRule m) d)

translateRule
  :: (Ord a, Functor f) => Map a b -> Rule (f a) -> Rule (f b)
translateRule m (Rule l r)   = Rule (translateWord m l) (translateWord m r)
translateRule m (DPRule l r) = DPRule (translateWord m l) (translateWord m r)

translateWord :: (Ord a, Functor f) => Map a b -> f a -> f b
translateWord m = fmap (translateSymbol m)

translateSymbol m s = case M.lookup s m of
    Just d  -> d
    Nothing -> error "symbol not found"


