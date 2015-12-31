{-# LANGUAGE TypeFamilies #-}
module Encode.BinaryEncoding
( EncBinary (..)
, DecBinary (..)
, mapSymbols
) where

import Ersatz.Bits
import Ersatz.Codec
import Ersatz.Equatable
import Encode.SizeableVar
import Encode.EQKnown

import Control.Monad (replicateM)
import Control.Applicative

import Data.Set as S
import Data.Map as M

{-
Encoding for Symbols as binary integers.

Size of Symbols: floor (log2 SignatureSize + 1)

Equality: equality in every variable

Equality with known values: equality

Must be instance of Codec
-}

data EncBinary = EncBinary Bits deriving Show
data DecBinary = DecBinary Integer deriving (Show, Eq, Ord)

instance Codec EncBinary where
    type Decoded EncBinary = DecBinary
    decode s (EncBinary b)     = DecBinary <$> decode s b
    encode   (DecBinary i)     = EncBinary (encode i)

instance Equatable EncBinary where
    EncBinary a === EncBinary b = a === b

instance SizeableVar EncBinary where
    createVar size varType = do
        vars <- replicateM (fromIntegral $ bitCount size) varType
        return $ EncBinary (Bits vars)

instance EQKnown EncBinary where
    a ===! b = a === encode b

mapSymbols :: (Ord f) => Set f -> Map f DecBinary
mapSymbols s = M.fromList $ zip ( S.toList s ) (fmap DecBinary [0..])
