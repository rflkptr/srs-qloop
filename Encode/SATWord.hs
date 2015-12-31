{-# LANGUAGE TypeFamilies #-}

module Encode.SATWord
( SATWord (..)
, getWord
, getEnd
) where

import Ersatz (false, (&&))
import Ersatz.Codec
import Ersatz.Equatable
import Control.Applicative ((<$>),(<*>))
import Data.List (length)
import Prelude hiding ((&&))

{-
    Polymorphic SAT words with or without abstract end positions
    Needs a type binding at the top level of an encoding function.
    By that binding it is determined (for example) which encoding is
    used for symbols and positions.

-}


--SATWord, with or without ending pos
data SATWord a b = WithEnd [a] b | NoEnd [a] deriving Show

instance (Codec a, Codec b) => Codec (SATWord a b) where
    type Decoded (SATWord a b) = SATWord (Decoded a) (Decoded b)
    decode s (WithEnd w p)  = WithEnd <$> decode s w <*> decode s p
    decode s (NoEnd w)      = NoEnd <$> decode s w
    encode   (WithEnd w p)  = WithEnd (encode w) (encode p)
    encode   (NoEnd w)      = NoEnd (encode w)

instance (Equatable a, Equatable b) => Equatable (SATWord a b) where
    (WithEnd w0 e0) === (WithEnd w1 e1) = w0 === w1 && e0 === e1
    (NoEnd w0)      === (NoEnd w1)      = w0 === w1
    _               === _               = false

getWord :: SATWord t t1 -> [t]
getWord (WithEnd  w _) = w
getWord (NoEnd w)      = w

getEnd :: SATWord t t1 -> Maybe t1
getEnd (WithEnd _ e) = Just e
getEnd (NoEnd _)     = Nothing
