module SRS.Term
( SRSTerm (..)
) where

import Data.Set as S
import Data.List as L
import Control.Applicative
import Data.Foldable as F
import Data.Monoid as M
{-
    A type class for structures which can act as terms 
    in the sense of string rewriting
    
    Excuse: I just want to overload operators, this should not be a type class
-}

class (Foldable t, Applicative t) => SRSTerm t where
    emptyWord       :: Monoid (t f) => t f
    root            :: t f -> Maybe f                                           -- ^ root symbol of the term
    asList          :: t f -> [f]                                               -- ^ transform term to a list of symbols
    prependSymbol   :: Monoid (t f) => f -> t f -> t f                          -- ^ add a symbol to the left
    appendSymbol    :: Monoid (t f) => t f -> f -> t f                          -- ^ add a symbol to the right
    append          :: Monoid (t f) => t f -> t f -> t f                        -- ^ concat two terms (~ substitution)
    subterms        :: Monoid (t f) => t f -> [(t f)]                           -- ^ collection of all subterms
    properSubterms  :: Monoid (t f) => t f -> [(t f)]                           -- ^ proper subterms (whole term is excluded)
    isSubterm       :: (Monoid (t f), Eq (t f)) => t f -> t f -> Bool           -- ^ test if t is subterm of s
    isProperSubterm :: (Monoid (t f), Eq (t f)) => t f -> t f -> Bool           -- ^ test if t is proper subterm of s
    symbolAt        :: Monoid (t f) => t f -> Int -> f                          -- ^ get symbol at term position i
    symbolCount     :: t f -> Int                                               -- ^ count the symbols in a term
    isEmpty         :: t f -> Bool                                              -- ^ test if term is empty
    filter          :: Monoid (t f) => (f -> Bool) -> t f -> t f                -- ^ filter symbols (elemts) from term

-- please overwrite this! For example the default implementation for symbolAt is
-- strongly depending on haskell preserving some kind of order on list elements
-- TODO: this is horrible
    emptyWord           = M.mempty
    root                = M.getFirst . F.foldMap (First . Just)
    asList              = F.toList
    prependSymbol       = \x y -> M.mappend (pure x) y
    appendSymbol        = \x y -> M.mappend x (pure y)
    append              = M.mappend
    subterms            = F.foldr (\x (y:ys) -> [prependSymbol x y] ++ (y:ys)) $
                          [emptyWord]
    properSubterms t    = case subterms t of {(x:xs) -> xs;[] -> []}
    isSubterm x y       = F.elem y (subterms x)
    isProperSubterm x y = F.elem y (properSubterms x)     
    symbolCount         = F.foldl' (\e _ -> e + 1) 0
    isEmpty             = F.foldr  (\_ _ -> False) True
    symbolAt t i        = 
        case (M.getFirst . F.foldMap (First . Just)) (subterms t !! i) of
            Just f -> f
            _      -> error "Index too large"
    filter p            = F.foldMap (\a -> if p a then pure a else mempty)

{-
    Instance for list
-}
instance SRSTerm [] where
    root (s:_)              = Just s
    root _                  = Nothing
    
    asList                  = id
    prependSymbol           = (:)
    appendSymbol l s        = l ++ [s] 
    append                  = (++)
    subterms                = L.tails
    
    properSubterms (x:xs)   = subterms xs
    properSubterms []       = []
    
    isSubterm s t           = L.elem t (subterms s)
    isProperSubterm s t     = L.elem t (properSubterms s)
    
    symbolAt                = (!!)
    symbolCount             = L.length
    
    isEmpty []              = True
    isEmpty _               = False
    
    filter                  = L.filter

