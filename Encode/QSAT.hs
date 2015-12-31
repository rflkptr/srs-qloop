{-# LANGUAGE TypeFamilies #-}

module Encode.QSAT
( qsolve
, qgetEncodingBS
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

import Prelude hiding ((&&),(||))

import CLI.CLI as C

{-
    For writing encodings in QDIMACS format
-}

qgetEncodingBS :: System [String] -> Encoding -> Flags -> QSATOptions -> BS.ByteString
qgetEncodingBS sys Binary flags options = qdimacsQSAT $ 
    qsatBinary flags options $ 
    translateSRS (BN.mapSymbols $ getSignature sys) sys

getEncodingBS sys Onehot flags options = qdimacsQSAT $ 
    qsatOnehot flags options $ 
    translateSRS (OH.mapSymbols $ getSignature sys) sys

getEncodingBS sys Order flags options = qdimacsQSAT $ 
    qsatOrder flags options $ 
    translateSRS (OR.mapSymbols $ getSignature sys) sys

{-
    Encode + solve for a given system and symbol encoding
-}

qsolve :: System [String] -> Encoding -> Flags -> QSATOptions -> IO ()
qsolve sys Binary flags options = let mapping = BN.mapSymbols (getSignature sys)
    in do
        (res, msol) <- solveWith depqbf $ 
            qsatBinary flags options $
            translateSRS mapping sys
        when (res /= Satisfied) (fail (show res))
        case msol of
            Just x -> putStrLn $ "SAT\n" ++ 
                      pretty sys ++ "\n" ++ 
                      (renderS mapping "_|_" x sys) 
            _      -> fail ("sol was " ++ show msol)

qsolve sys Onehot flags options = let mapping = OH.mapSymbols (getSignature sys)
    in do
        (res, msol) <- solveWith depqbf $ 
            qsatOnehot flags options $
            translateSRS mapping sys
        when (res /= Satisfied) (fail (show res))
        case msol of
            Just x -> putStrLn $ "SAT\n" ++ 
                      pretty sys ++ "\n" ++ 
                      (renderS mapping "_|_" x sys) 
            _      -> fail ("sol was " ++ show msol)

qsolve sys Order flags options = let mapping = OR.mapSymbols (getSignature sys)
    in do
        (res, msol) <- solveWith depqbf $ 
            qsatOrder flags options $
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
   
qsatBinary :: (HasQSAT s, MonadState s m, Applicative m) 
    => Flags
    -> QSATOptions 
    -> System [DecBinary]
    -> m [DeriveStep EncBinary Bits Bit]
qsatBinary flags options sys = do
    (f, l) <- qloop 
        sys
        ((S.size $ getSignature sys) - 1)
        (quantorPairs options)
        ((wordLength . qdim) options)
        ((derivationSteps . qdim) options)
        flags
    assert f
    return l

{-
    For onehot encoded symbols call this with properly translated system
    This encoding also needs to assert that all symbols are in the signature
-}

qsatOnehot :: (HasQSAT s, MonadState s m, Applicative m) 
    => Flags
    -> QSATOptions
    -> System [DecOneHot]
    -> m [DeriveStep EncOneHot Bits Bit]
qsatOnehot flags options sys = do
    (f, l) <- qloop 
        sys
        (S.size $ getSignature sys)
        (quantorPairs options)
        ((wordLength . qdim) options)
        ((derivationSteps . qdim) options)
        flags
    assert $ Ersatz.and $ fmap (insignature sys) l
    assert f
    return l

{-
    For onehot encoded symbols call this with properly translated system
    This encoding also needs to assert that all symbols are in the signature
-}

qsatOrder :: (HasQSAT s, MonadState s m, Applicative m) 
    => Flags
    -> QSATOptions
    -> System [DecOrder]
    -> m [DeriveStep EncOrder Bits Bit]
qsatOrder flags options sys = do
    (f, l) <- qloop 
        sys
        (S.size $ getSignature sys)
        (quantorPairs options)
        ((wordLength . qdim) options)
        ((derivationSteps . qdim) options)
        flags
    assert $ Ersatz.and $ fmap (insignature sys) l
    assert f
    return l


{-
    Formula to make sure that all symbols used in a step are in the signature
-}

insignature sys = 
    Ersatz.and . (fmap (insig (getSignature sys))) . getWord . word

insig sig l = Ersatz.any (\s -> l === (encode s)) sig

{-
    Abstract encoding for loops in SRS by QSAT encoding
-}

qloop :: (SizeableVar w, EQKnown w, HasQSAT s, MonadState s m, Ord a,
                     Num a1, Num a, Integral a3, Integral a2, Eq a1) 
    => System [(Decoded w)]
    -> a3 
    -> a1 
    -> a2 
    -> a 
    -> Flags 
    -> m (Bit, [DeriveStep w Bits b])

qloop sys sigSize cutOff length steps flags = do
    s       <- mkSATWord length sigSize exists $
              (lengthPreserving sys && check_LP flags)
    t       <- mkSATWord length sigSize exists $
              (lengthPreserving sys && check_LP flags)
    (f,l)   <- qreach s t sys sigSize cutOff length steps flags True
    
    let endloop = (constrainEnd (constrainEnd (substring s t sys) t length) s length)
    let floop   = wordEnds (endloop) l length
--    let f' = L.foldr (\step form -> constrainEnd form (word step) length) endloop l
    return $ (floop && f, (Step s Nothing Nothing) : l ++ [(Step t Nothing Nothing)])


wordEnds f l len = L.foldl' (\f' (Step w _ _) -> case w of
        WithEnd _ e -> f' && (e <? encode (fromIntegral len))
        NoEnd _     -> f'
    ) f l
{-
    Recursive QSAT encoding by branching for words in the middle of a (long)
    derivation.
    
-}

qreach :: (SizeableVar t, EQKnown t, HasQSAT s, MonadState s m, Ord a,
            Num a1, Num a, Integral a3, Integral a2, Eq a1)
    => SATWord t Bits
    -> SATWord t Bits
    -> System [(Decoded t)]
    -> a3
    -> a1
    -> a2
    -> a
    -> Flags
    -> Bool
    -> m (Bit, [DeriveStep t Bits b])
qreach s t sys sigSize cutOff length steps flags inEqFlag =
    if steps < 1
        then do
            pos   <- bitsVar (bitCount $ (symbolCount (getWord s)) - 1) exists
            rbits <- mkRBits 0 (ruleCount sys) exists
            lc    <- get_left_copy s t
            dc    <- cp_delta_classes s t (catRules sys) (M.empty)
            if inEqFlag
                then return $ 
                    ((application lc dc s t pos (M.assocs rbits) sys),[])
                else return $ 
                    ((application lc dc s t pos (M.assocs rbits) sys) || (s === t),[])
        else if cutOff == 0
            then do
                -- if cutOff == 0 -> no more quantifies, so build a SAT formula
                m        <- mkSATWord length sigSize exists $
                           (lengthPreserving sys && check_LP flags)
                (fl,l1)  <- qreach s m sys sigSize 0 length (steps - 1) flags False
                (fr,l2)  <- qreach m t sys sigSize 0 length (steps - 1) flags False
                if inEqFlag
                    then return $ ( Ersatz.not (getWord m === getWord s) && 
                                    (fl && fr), 
                                    l1 ++ ((Step m Nothing Nothing):l2) )
                    else return $ (fl && fr, l1 ++ (Step m Nothing Nothing):l2)
                
            else do
                -- exists m, forall a,b: ...
                m   <- mkSATWord length sigSize exists $
                       (lengthPreserving sys && check_LP flags)
                a   <- mkSATWord length sigSize forall $
                       (lengthPreserving sys && check_LP flags)
                b   <- mkSATWord length sigSize forall $
                       (lengthPreserving sys && check_LP flags)
                
                (fbranch,_) <- qreach a b sys sigSize (cutOff - 1) length (steps - 1) flags False
                let f = a === s && b === m || a === m && b === t ==> fbranch
                if inEqFlag
                    then return (f && Ersatz.not (getWord m === getWord s), [(Step m Nothing Nothing)])
                    else return (f, [(Step m Nothing Nothing)])


{-
    Constrain abstract word ends to be smaller than the word length if existing
-}

constrainEnd f (WithEnd _ e) max  = f && (e <? encode (fromIntegral max))
constrainEnd f _ _                = f
 
