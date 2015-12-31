module SRS.System.Type
( System (..)
) where

import SRS.Rule (Rule)

data System t = SRS [Rule t] | DPProblem [Rule t] [Rule t]
    deriving (Show, Eq, Ord)
