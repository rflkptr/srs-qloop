{-# LANGUAGE FlexibleInstances #-}

module Pretty.Pretty
( Pretty (..)
) where

import SRS.Rule.Type
import SRS.System.Type

class PrettySymbol s where
    prettySymbol :: s -> String

instance PrettySymbol Char where
    prettySymbol c = [c]

instance PrettySymbol String where
    prettySymbol = id



class Pretty t where
    pretty :: t -> String

instance (PrettySymbol s) => Pretty [s] where
    pretty (s:ss) = (prettySymbol s) ++ "(" ++ pretty ss ++ ")"
    pretty [] =  "x" 

instance PrettySymbol t => Pretty (Rule [t]) where
    pretty (Rule l r)             = pretty l ++ " -> " ++ pretty r
    pretty (DPRule (l:ls) (r:rs)) = (prettySymbol l) ++ "#(" ++ pretty ls 
                                    ++ ") -> " ++ (prettySymbol r) 
                                    ++ "#(" ++ pretty rs ++ ")"
    pretty (DPRule (l:ls) [])     = (prettySymbol l) ++ "#(" ++ pretty ls ++ ") -> x"
    pretty (DPRule [] (r:rs))     = "x -> " ++ (prettySymbol r) ++ "#(" ++ pretty rs ++ ")"
    pretty _                      = ""

instance PrettySymbol t => Pretty (System [t]) where
    pretty (SRS rs)          = "Rules\n" ++ unlines (fmap pretty rs)
    pretty (DPProblem rs ds) = "Rules\n" ++ unlines (fmap pretty rs) ++ "\nDP\n" ++ unlines (fmap pretty ds)
