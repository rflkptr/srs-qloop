{-# LANGUAGE TypeFamilies #-}

module Encode.SAT
( solve
, getEncodingBS
) where

import Ersatz

import SRS.System

import Pretty.Pretty

import Encode.SizeableVar
import Encode.EQKnown
import Encode.BinaryEncoding as BN
import Encode.OneHotEncoding as OH
import Encode.OrderEncoding as OR
import Encode.SATWord
import Encode.DeriveStep
import Encode.OneStep


import Control.Monad.State
import Control.Applicative

import Data.Set as S
import Data.List as L
import Data.Map as M
import qualified Data.ByteString.Lazy as BS

import Prelude hiding ((&&), (||))

import CLI.CLI as C

{-
    For writing encodings in DIMACS format
-}

getEncodingBS :: System [String] -> Encoding -> Flags -> SATOptions -> BS.ByteString
getEncodingBS sys Binary flags options = dimacsSAT $ 
    satBinary flags options $ 
    translateSRS (BN.mapSymbols $ getSignature sys) sys

getEncodingBS sys Onehot flags options = dimacsSAT $ 
    satOnehot flags options $ 
    translateSRS (OH.mapSymbols $ getSignature sys) sys

getEncodingBS sys Order flags options = dimacsSAT $ 
    satOrder flags options $ 
    translateSRS (OR.mapSymbols $ getSignature sys) sys

{-
    Encode + solve for a given system and symbol encoding
-}

solve :: System [String] -> Encoding -> Flags -> SATOptions -> IO ()
solve sys Binary flags options = 
    let mapping = BN.mapSymbols (getSignature sys) 
    in do
        (res, msol) <- solveWith minisat $
            satBinary flags options $ 
            translateSRS mapping sys
        when (res /= Satisfied) (fail (show res))
        case msol of
            Just x -> putStrLn $ "SAT\n" ++ 
                      pretty sys ++ "\n" ++ 
                      (renderS mapping "_|_" x sys) 
            _      -> fail ("sol was " ++ show msol)

solve sys Onehot flags options = 
    let mapping = OH.mapSymbols (getSignature sys) 
    in do
        (res, msol) <- solveWith minisat $ 
            satOnehot flags options $ 
            translateSRS mapping sys
        when (res /= Satisfied) (fail (show res))
        case msol of
            Just x -> putStrLn $ "SAT\n" ++ 
                      pretty sys ++ "\n" ++ 
                      (renderS mapping "_|_" x sys) 
            _      -> fail ("sol was " ++ show msol)

solve sys Order flags options = 
    let mapping = OR.mapSymbols (getSignature sys) 
    in do
        (res, msol) <- solveWith minisat $ 
            satOrder flags options $ 
            translateSRS mapping sys
        when (res /= Satisfied) (fail (show res))
        case msol of
            Just x -> putStrLn $ "SAT\n" ++ 
                      pretty sys ++ "\n" ++ 
                      (renderS mapping "_|_" x sys) 
            _      -> fail ("sol was " ++ show msol)

{-
    For binary encoded symbols call this with properly translates system
-}

satBinary :: (HasSAT s, MonadState s m, Applicative m) 
    => Flags 
    -> SATOptions 
    -> System [DecBinary]
    -> m [DeriveStep EncBinary Bits Bit]
satBinary flags options sys = do
    (f, l) <- satEncoding 
        sys
        ((S.size $ getSignature sys) - 1)
        ((wordLength . sdim) options)
        ((derivationSteps . sdim) options)
        flags
    if (lengthPreserving sys && check_LP flags)
        then assert f
        else assert $ 
            Ersatz.and (wordEnds l ((wordLength . sdim) options)) && f
    return l

{-
    For onehot encoded symbols call this with properly translates system
    This encoding also needs to assert that all symbols are in the signature
-}

satOnehot :: (HasSAT s, MonadState s m, Applicative m) 
    => Flags 
    -> SATOptions 
    -> System [DecOneHot]
    -> m [DeriveStep EncOneHot Bits Bit]
satOnehot flags options sys = do
    (f, l) <- satEncoding 
        sys
        (S.size $ getSignature sys)
        ((wordLength . sdim) options)
        ((derivationSteps . sdim) options)
        flags
    assert $ Ersatz.and $ fmap (insignature sys) l
    if (lengthPreserving sys && check_LP flags)
        then assert f
        else assert $ 
            Ersatz.and (wordEnds l ((wordLength . sdim) options)) && f
    return l

{-
    For onehot encoded symbols call this with properly translates system
    This encoding also needs to assert that all symbols are in the signature
-}

satOrder :: (HasSAT s, MonadState s m, Applicative m) 
    => Flags 
    -> SATOptions 
    -> System [DecOrder]
    -> m [DeriveStep EncOrder Bits Bit]
satOrder flags options sys = do
    (f, l) <- satEncoding 
        sys
        (S.size $ getSignature sys)
        ((wordLength . sdim) options)
        ((derivationSteps . sdim) options)
        flags
    assert $ Ersatz.and $ fmap (insignature sys) l
    if (lengthPreserving sys && check_LP flags)
        then assert f
        else assert $ 
            Ersatz.and (wordEnds l ((wordLength . sdim) options)) && f
    return l

{-
    Formula to make sure that all symbols used in a step are in the signature
-}

insignature sys = 
    Ersatz.and . (fmap (insig (getSignature sys))) . getWord . word

insig sig l = Ersatz.any (\s -> l === (encode s)) sig

{-
    If word ends exist (no length preserving system + option)
    then a formula is constructed which makes sure that all word ends are
    smaller than the maximum word length    
-}

wordEnds ((Step w _ _):s) len = case w of
    WithEnd _ e -> (e <? encode (fromIntegral len)) : (wordEnds s len)
    _ -> error "should never happen"
wordEnds [] _ = []

{-
    Abstract encoding for loops in SRS by SAT encoding
-}

satEncoding :: ( SizeableVar w, EQKnown w, HasSAT s, MonadState s m, Num a,
                      Integral a1, Integral a2, Eq a ) 
    => System [(Decoded w)]
    -> a2 
    -> a1 
    -> a 
    -> Flags 
    -> m (Bit, [DeriveStep w Bits Bit])

satEncoding sys sigSize length steps flags = do
    start  <- mkSATWord length sigSize exists $
              (lengthPreserving sys && check_LP flags)
    (f,l)  <- satloop sys sigSize start start length steps flags
    return $ (f, (Step start Nothing Nothing) : l)

{-
    RECursive SAT encoding
-}

satloop :: ( SizeableVar w, EQKnown w, HasSAT s, MonadState s m, Num a,
            Integral a1, Integral a2, Eq a ) 
    => System [(Decoded w)]
    -> a2
    -> SATWord w Bits
    -> SATWord w Bits
    -> a1
    -> a
    -> Flags
    -> m (Bit, [DeriveStep w Bits Bit])

satloop sys sigSize start current length steps flags = if steps == 0
    then return (false, []) 
    else do
        next  <- mkSATWord length sigSize exists $
                 (lengthPreserving sys && check_LP flags)
        pos   <- bitsVar (bitCount $ (symbolCount (getWord next)) - 1) exists
        rbits <- mkRBits 0 (ruleCount sys) exists
        lc    <- get_left_copy current next
        dc    <- cp_delta_classes current next (catRules sys) (M.empty)
        (f,l) <- satloop sys sigSize start next length (steps - 1) flags
        let f' = --constrainEnd next length $
                 (application lc dc current next pos (M.assocs rbits) sys) &&
                 ((substring start next sys) || f)
        return $ (f', (Step next (Just pos) (Just rbits)):l)

{-
    Constrain abstract word ends to be smaller than the word length if existing
-}

constrainEnd (WithEnd w e) max f = (e <? encode (fromIntegral max)) && f
constrainEnd _ _ f               = f

