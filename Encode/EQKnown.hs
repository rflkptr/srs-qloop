module Encode.EQKnown
( EQKnown (..)
) where

import Prelude hiding ((&&), and)
import Ersatz (and)
import Ersatz.Bit (Bit, (&&), true, false)
import Ersatz.Bits (Bits)
import Ersatz.Codec (Codec, Decoded, encode)
import Ersatz.Equatable (Equatable, (===))

class (Equatable t, Codec t) => EQKnown t where
    (===!) :: t -> (Decoded t) -> Bit

instance EQKnown t => EQKnown [t] where
    x ===! y 
        | length x /= length y = false
        | otherwise            = and $ map (uncurry (===!)) (zip x y)

instance EQKnown Bits where
    x ===! y = x === (encode y)
