{-# LANGUAGE TypeFamilies #-}

module Encode.OneStep where

-- encoding for rule application (one step relation) 

import Prelude hiding ((&&),(||))

import Ersatz

import Encode.SizeableVar
import Encode.EQKnown
import Encode.SATWord
import SRS.System

import SRS.Term

import Control.Monad.State
import Control.Monad (replicateM)

import Data.Map as M 
import Data.List as L

type Position = Int

{-
    creates as fresh SATWord
    Reservate *length* variable vectors which have the size *sigSize* to
    hold any symbol of a given alphabet in a certain encoding. 
    Any of these variables are quantified by *mkVar*.
    The flag is used to determine if an abstract word end should be reservated.
    These abstract word ends are binary encoded natural numbers (TODO: for now) 
-}

mkSATWord :: (SizeableVar a, Monad m, Integral a2, Integral a1) =>
     a1 -> a2 -> m Bit -> Bool -> m (SATWord a Bits)

mkSATWord length sigSize mkVar False = do
    word <- replicateM (fromIntegral length) (createVar (sigSize) mkVar)
    end  <- bitsVar (bitCount $ ((symbolCount word) - 1)) mkVar 
    return $ WithEnd word end
        
mkSATWord length sigSize mkVar True = do
    word <- replicateM (fromIntegral length) (createVar (sigSize) mkVar)
    return $ NoEnd word

{-
    Result is a bit variable vector indicating which rule should get applied
-}

mkRBits :: (Monad m) => Int -> Int -> m Bit -> m (Map Int Bit)
mkRBits from to mkVar = if (to > 0)
    then do
        bits <- replicateM (fromIntegral to) mkVar
        return $ M.fromList ( zip [from .. ((from + to) - 1)] bits )
    else error "Rule map is empty"

-- full rule application formula
application :: 
    (EQKnown t2, EQKnown f, Boolean t1, Equatable t1, Decoded t2 ~ Decoded f) 
    => [Bit]
    -> Map Int [Bit]
    -> SATWord f Bits
    -> SATWord t2 Bits
    -> Bits
    -> [(Int, t1)]
    -> System [Decoded f]
    -> Bit

application lc dc word result abstract_pos rbits sys = 
    Ersatz.any (\(rid,flag) ->
        flag === true && 
            (ruleApp 
                (app_delta lc dc) 
                word 
                result 
                abstract_pos 
                ((catRules sys) !! rid) 
                sys
            )
    ) rbits

{-
    Rule application for certain Position
-}

ruleApp :: ([f] -> [t] -> [a] -> [a] -> Position -> Bit)
     -> SATWord f Bits
     -> SATWord t Bits
     -> Bits
     -> Rule [a]
     -> System t1
     -> Bit

ruleApp app (WithEnd w0 e0) (WithEnd w1 e1) pos (Rule l r) (SRS _) =
    Ersatz.or $ do
        p <- [0 .. ((symbolCount w0 - 1) - (lrbound l r))]
        return $ pos === (encode $ fromIntegral p) &&
            (app w0 w1 l r p) && (adjustWE e0 e1 pos l r)

ruleApp app (WithEnd w0 e0) (WithEnd w1 e1) pos (Rule l r) (DPProblem _ _) =
    Ersatz.or $ do
        p <- [1 .. ((symbolCount w0 - 1) - (lrbound l r))]
        return $ pos === (encode $ fromIntegral p) &&
            (app w0 w1 l r p) && (adjustWE e0 e1 pos l r)

ruleApp app (WithEnd w0 e0) (WithEnd w1 e1) pos (DPRule l r) (DPProblem _ _) =
    (pos === encode 0) && (app w0 w1 l r 0) && (adjustWE e0 e1 pos l r)

ruleApp app (NoEnd w0) (NoEnd w1) pos (Rule l r) (SRS _) =
    Ersatz.or $ do
        p <- [0 .. ((symbolCount w0 - 1) - (lrbound l r))]
        return $ pos === (encode $ fromIntegral p) &&
            (app w0 w1 l r p)

ruleApp app (NoEnd w0) (NoEnd w1) pos (Rule l r) (DPProblem _ _) = 
    Ersatz.or $ do
        p <- [1 .. ((symbolCount w0 - 1) - (lrbound l r))]
        return $ pos === (encode $ fromIntegral p) &&
            (app w0 w1 l r p)

ruleApp app  (NoEnd w0) (NoEnd w1) pos (DPRule l r) (DPProblem _ _) =
    (pos === encode 0) && (app w0 w1 l r 0)

ruleApp _ _ _ _ _ _ = error "invalid rule application for known positions"

-- for constraining positions by rule length
lrbound :: [a] -> [a] -> Int
lrbound l r = (maximum [symbolCount l, symbolCount r]) - 1

app_lp :: (EQKnown t1, EQKnown t) =>
     [Bit]
     -> Map LengthPair [Bit]
     -> [t]
     -> [t1]
     -> [Decoded t]
     -> [Decoded t1]
     -> Position
     -> Bit

app_lp lc dc x y l r p =
    case M.lookup (get_lp' l r) dc of
        Just rc -> Ersatz.and [ match x p l
                              , match y p r
                              , (lc !! p)
                              , (rc !! p)
                              ]
        Nothing -> error "Missing lengthpair"


app_delta lc dc x y l r p =
    case M.lookup (get_delta (Rule l r)) dc of
        Just rc -> Ersatz.and [ match x p l
                              , match y p r
                              , (lc !! p)
                              , if L.length rc <= (p + (symbolCount l))
                                    then true
                                    else rc !! (p + (symbolCount l))
                              ]
        Nothing -> error $ show $ get_delta (Rule l r)


{-
    match for a position known at encode time
-}
match :: EQKnown t => [t] -> Position -> [Decoded t] -> Bit
match w0 pos w1 = Ersatz.and $ do
    (i,s) <- zip [pos .. (pos + (symbolCount w1))] w1
    if (i < symbolCount w0)
        then return $ (w0 !! i) ===! s
        else return false

-- Copy by use of equivalence classes ------------------------------------------
-- Refined copy function with help variables to make encodings of linear size --

{-  ----- Copy positions smaller than i -----
    Create help variables h_{i}(x,y), 0 <= i < max_word_len which imply
    that when h_{i}(x,y) is true, all positions in the words x y smaller than i
    must be the same symbols
    
    h_{0}(x,y) => true // nothing needs to be copied
    h_{i}(x,y) => h_{i-1} && x_{i-1} = y_{i-1}  //copy everything smaller i
-}
get_left_copy :: (Equatable t, HasSAT s, MonadState s m) 
    => SATWord t t1 
    -> SATWord t t2 
    -> m [Bit]

get_left_copy x y = 
    let max = (symbolCount . getWord) x
    in do
        cbits <- replicateM max exists
        assertLeftCPs cbits max x y
        return $ cbits

assertLeftCPs :: (Equatable t, HasSAT s, MonadState s m) 
    => [Bit] 
    -> Int 
    -> SATWord t t1 
    -> SATWord t t2 
    -> m ()

assertLeftCPs flags max x y = sequence_ $ do
    p <- [0 .. (max-1)]
    return (assertLeftCP flags p x y)

assertLeftCP flags 0 x y = assert $ (flags !! 0) ==> true

assertLeftCP flags i x y = let dec = i - 1
    in assert $ (flags !! i) ==> (flags !! dec) && 
        ((getWord x !! dec) === (getWord y !! dec))

-- Copy positions greater/equal than i grouped by length pair eq classes -------

{- length pairs for when rules get grouped into equivalence classes where
   l -> r = u -> v <=> |l|=|u| && |r|=|v|
-}
data LengthPair = LengthPair 
    { lsize :: Int 
    , rsize :: Int
    } deriving (Show,Eq,Ord)

get_lp rule = LengthPair ((symbolCount . lhs) rule) ((symbolCount . rhs) rule)
get_lp' l r = LengthPair (symbolCount l) (symbolCount r)

copy_lp_classes :: (Equatable t1, HasSAT s, MonadState s m, SRSTerm t) 
    => SATWord t1 t2
    -> SATWord t1 t3
    -> [Rule (t f)]
    -> Map LengthPair [Bit]
    -> m (Map LengthPair [Bit])
     
copy_lp_classes x y [] eqclass = return eqclass
copy_lp_classes x y (r:rs) eqclass = 
    let lp = get_lp r
    in case M.lookup lp eqclass of
        Just _ -> copy_lp_classes x y rs eqclass
        Nothing -> do
            del <- get_lp_copy x y lp
            copy_lp_classes x y rs (M.insert lp del eqclass)

{-
    Create help variables h_{l,r,p}(x,y) such that
    h_{l,r,p)(x,y) => x_{p+|l|} = y_{p+|r|} && h_{l,r,p+1)(x,y) if p < |y| and p + |l| < |y| 
-}

get_lp_copy :: (Equatable t, HasSAT s, MonadState s m) 
    => SATWord t t1 
    -> SATWord t t2 
    -> LengthPair 
    -> m [Bit]

get_lp_copy x y (LengthPair ls rs) = 
    let maxlen = (symbolCount . getWord) x
    in do
       cbits <- replicateM maxlen exists
       assert_offsets cbits maxlen x y ls rs
       return $ cbits

assert_offsets flags m x y ls rs = sequence_ $ do
    p <- [0 .. (m-1)]
    return (assert_offset flags p m x y ls rs)

assert_offset flags 0 m x y ls rs = if (max ls rs) >= m
    then assert $ flags !! 0 ==> true
    else assert $ (flags !! 0) ==> (flags !! 1) && 
        ((getWord x !! ls) === (getWord y !! rs))

assert_offset flags i m x y ls rs = let diff = i + (max ls rs)
    in if diff >= m
        then if i + 1 < m
            then assert $ flags !! i ==> (flags !! (i+1))
            else assert $ flags !! i ==> true 
        else assert $ (flags !! i) ==> (flags !! (i+1)) && 
            ((getWord x !! (i+ ls)) === (getWord y !! (i + rs))) 

-- /Copy positions greater/equal than i grouped by length pair eq classes ------
-- Copy positions greater/equal than i grouped by delta eq classes -------------
{-
    Equivalence classes where l -> r = u -> v <=> |r| - |l| = |v| - |u|
-}
cp_delta_classes :: (Equatable t3, HasSAT s, MonadState s m, SRSTerm t) 
    => SATWord t3 t2
    -> SATWord t3 t1
    -> [Rule (t f)]
    -> Map Int [Bit]
    -> m (Map Int [Bit])

cp_delta_classes x y [] eqclass     = return eqclass
cp_delta_classes x y (rule:rs) eqclass = 
    let delta = get_delta rule
    in case M.lookup delta eqclass of
        Just _ -> cp_delta_classes x y rs eqclass
        Nothing -> do
            del <- get_delta_copy x y delta
            cp_delta_classes x y rs (M.insert delta del eqclass)

get_delta rule = ((symbolCount . rhs) rule) - ((symbolCount . lhs) rule)

copy_func_delta from to delta p  
        | target > maxlen  = true
        | target < 0       = false
        | otherwise        = (x !! p) === (y !! target)
    where x      = getWord from
          y      = getWord to
          maxlen = (symbolCount y) - 1
          target = p + delta
{-
    Create help variables h_{delta,p}(x,y) such that
    h_{delta,p)(x,y) => x_{p} = y_{p+delta} && h_{delta,p+1)(x,y) 
        if p < |y| and p + |l| < |y| 
-}

get_delta_copy :: (Equatable t, HasSAT s, MonadState s m) 
    => SATWord t t2 
    -> SATWord t t1 
    -> Int 
    -> m [Bit]
    
get_delta_copy x y delta = 
    let m = (symbolCount . getWord) x
    in do
       cbits <- replicateM m exists
       assert_deltas x y cbits m delta
       return $ cbits

assert_deltas x y vars max_l delta = 
    let cp_func = copy_func_delta x y delta 
    in sequence_ $ do
        p <- [0 .. (max_l-1)]
        return (assert_delta cp_func vars max_l p)

assert_delta copy_func vars max_l p = if (p + 1) > (max_l - 1)
    then assert $ (vars !! p) ==> (copy_func p)
    else assert $ (vars !! p) ==> (copy_func p) && (vars !! (p+1))

-- /Copy positions greater/equal than i grouped by delta eq classes ------------
-- /Copy by use of equivalence classes -----------------------------------------

-- Adjusting abstract word ends ------------------------------------------------

{-
    adjust abstract word ends
-}
adjustWE :: Bits -> Bits -> Bits -> [a] -> [b] -> Bit
adjustWE e0 e1 pos l r = (endBound e0 e1 l r) && (piBound e0 pos (wlen l))

{-
    e_i+1 + |l|  == e_i + |r|
    but because of indices beginning at 0, this case must be handled
    (because that way we have no indicator that a rule application results in
    the empty word)
-}

endBound e0 e1 l r = case (symbolCount l, symbolCount r) of
    (0,0) -> error "rule x -> x"
    (_,0) -> (e0 === (wlen l) ==> false) && (e1 + (wlen l) + encode 1 === e0)
    (0,_) -> e1 === e0 + (wlen r) + encode 1
    (_,_) -> e1 + (wlen l) === e0 + (wlen r)
    
{- 
    ei >= pi + |l| 
-}

piBound :: Bits -> Bits -> Bits -> Bit
piBound end pos length = end >=? pos + length

-- /Adjusting abstract word ends -----------------------------------------------

-- Substrings ------------------------------------------------------------------
{-
    formula for substrings depending on the kind of SATWords and the
    underlying SRS type.

-}

substring :: (Orderable p, Equatable w, Codec p, Decoded p ~ Integer) =>
     SATWord w p -> SATWord w p -> System (t t1) -> Bit

substring (WithEnd w0 e0) (WithEnd w1 e1) (DPProblem _ _) = 
    e0 <=? e1 && (Ersatz.all (\(s,s') -> s === s') (zip w0 w1))

substring (NoEnd w0) (NoEnd w1) _ = w0 === w1

substring (WithEnd w0 e0) (WithEnd w1 e1) _ = 
    let widthW0 = intLen w0 - 1
        widthW1 = intLen w1 - 1
    in e0 <=? e1 && Ersatz.any (\start ->
            Ersatz.all (\j -> 
                ( encode j ) <=? e0 ==>           
                    if (start + j) <= widthW1
                        then encode (start + j) <=? e1 && 
                             getSymbol w0 j === getSymbol w1 (start + j)       
                        else false
            ) [0..widthW0]
        ) [0..widthW1]

substring _ _ _ = error "invalid invokation for substring"

-- /Substrings -----------------------------------------------------------------

--- utility

wlen :: [a] -> Bits
wlen [] = 0
wlen w  = encode $ fromIntegral $ (symbolCount w) - 1

intLen :: [a] -> Integer
intLen [] = 0
intLen (a:ts) = 1 + intLen ts

getSymbol :: [a] -> Integer -> a
getSymbol l j = l !! (fromIntegral j) 
