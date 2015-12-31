{-# LANGUAGE TypeFamilies #-}
module Encode.OrderEncoding
( EncOrder (..)
, DecOrder (..)
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

Size of Symbols: SignatureSize - 1

Must be instance of Codec
-}

data EncOrder = EncOrder [Bit]  deriving Show
data DecOrder = DecOrder [Bool] deriving (Show, Eq, Ord)

instance Codec EncOrder where
    type Decoded EncOrder = DecOrder
    decode s (EncOrder b)     = DecOrder <$> decode s b
    encode   (DecOrder i)     = EncOrder (encode i)

instance Equatable EncOrder where
    EncOrder a === EncOrder b = a === b

instance SizeableVar EncOrder where
    createVar size varType = do
        vars <- replicateM (fromIntegral (size - 1)) varType
        return $ EncOrder vars

instance EQKnown EncOrder where
    a ===! b = a === (encode b)


mapSymbols :: (Ord f) => Set f -> Map f DecOrder
mapSymbols s = let max = S.size s - 1
    in M.fromList $
        zip (S.toList s) (getOrd max)

getOrd max = [DecOrder ([a < b | a <- [1..max]]) | b <- [1..max] ] 
    ++ [DecOrder (replicate max True)]


