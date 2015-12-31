{-# LANGUAGE TypeFamilies #-}
module Encode.OneHotEncoding
( EncOneHot (..)
, DecOneHot (..)
, mapSymbols
) where

import Prelude hiding ((&&))
import Ersatz (and, (&&))
import Ersatz.Bit
import Ersatz.Codec
import Ersatz.Equatable
import Encode.SizeableVar
import Encode.EQKnown

import Control.Monad (replicateM)
import Control.Applicative

import Data.Set as S
import Data.Map as M

{-
Encoding for Symbols as bit vectors.

Size of Symbols: SignatureSize

Equality is equality in every variable

Equality with known values: 
is equality of first occurence of True in the encoding of the known value

Must be instance of Codec
-}

data EncOneHot = EncOneHot [Bit]  deriving Show
data DecOneHot = DecOneHot [Bool] deriving (Show, Eq, Ord)

instance Codec EncOneHot where
    type Decoded EncOneHot = DecOneHot
    decode s (EncOneHot b)     = DecOneHot <$> decode s b
    encode   (DecOneHot i)     = EncOneHot (encode i)

instance Equatable EncOneHot where
    EncOneHot a === EncOneHot b = a === b

instance SizeableVar EncOneHot where
    createVar size varType = do
        vars <- replicateM (fromIntegral size) varType
        return $ EncOneHot vars
{-
instance EQKnown EncOneHot where
    EncOneHot a ===! DecOneHot b = let pos = truePos 0 b
        in if pos >= length a
            then false
            else a !! pos === true

-}
instance EQKnown EncOneHot where
    EncOneHot a ===! DecOneHot b = let pos = truePos 0 b
        in if pos >= length a
            then false
            else if pos > 0
                then Ersatz.and [(a !! i === false) | i <- [0..pos-1]] && 
                     (a !! pos === true)
                else a !! 0 === true

                
truePos _ [] = error $ "No true value found"
truePos q (True:bs)  = q
truePos q (False:bs) = truePos (q+1) bs


mapSymbols :: (Ord f) => Set f -> Map f DecOneHot
mapSymbols s = M.fromList $ 
    fmap (\(a,b) -> (a,DecOneHot $ fmap (==b) [1..(S.size s)]) ) 
        $ (zip (S.toList s) [1..(S.size s)])

