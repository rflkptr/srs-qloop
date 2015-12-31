module Encode.SizeableVar
( SizeableVar (..)
, bitsVar
, bitCount
) where

import Ersatz.Bit (Bit)
import Ersatz.Bits
import Control.Monad (replicateM)

class SizeableVar t where
    -- | creates variables of type t with a certain size 
    createVar :: (Monad m, Integral a) 
        => a        -- ^ size 
        -> m Bit    -- ^ exists, forall?
        -> m t      -- ^ variables

{-
    TODO: Belongs to position encoding
    
-}
bitsVar :: 
    (Monad m, Integral a)
    => a     -- ^ size of the bit vector
    -> m Bit -- ^ what type the variable will be of (exists, forall)
    -> m Bits
bitsVar size varType = do
    vars <- replicateM (fromIntegral size) varType
    return $ Bits vars

bitCount :: (Integral a) => a -> Int
bitCount n = if n == 0
    then 1
    else fromIntegral $ floor ( (log (fromIntegral n) / log 2) + 1 )
