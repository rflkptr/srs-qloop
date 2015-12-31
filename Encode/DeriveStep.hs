{-# LANGUAGE TypeFamilies #-}

module Encode.DeriveStep
( DeriveStep (..)
, renderSteps
, renderS
) where

import Ersatz.Codec
import Ersatz.Bit (Bit)
import Encode.SATWord
import SRS.Rule
import SRS.System
import Data.Map as M

import Control.Applicative ((<$>),(<*>))

data DeriveStep w p b = Step 
    { word :: (SATWord w p)
    , pos  :: Maybe p
    , ruleVector :: Maybe (Map Int b)
    } deriving Show


instance Codec t => Codec (Rule t) where
    type Decoded (Rule t) = Rule (Decoded t)
    decode s (Rule l r) = Rule <$> decode s l <*> decode s r
    decode s (DPRule l r) = DPRule <$> decode s l <*> decode s r
    encode (Rule l r) = Rule (encode l) (encode r)
    encode (DPRule l r) = DPRule (encode l) (encode r)

instance (Codec w, Codec p, Codec b) => Codec (DeriveStep w p b) where
    type Decoded (DeriveStep w p b) = DeriveStep (Decoded w) (Decoded p) (Decoded b)
    decode s (Step wrd ps rv)   = Step 
        <$> decode s wrd 
        <*> decode s ps 
        <*> decode s rv
    
    encode (Step wrd ps rv)     = Step 
        (encode wrd) 
        (encode ps) 
        (encode rv)


renderDPWord m bot (w:[]) = (translateBot m bot w) ++ "#"
renderDPWord m bot (w:ws) = 
    (translateBot m bot w) ++ "#" ++ "," ++ renderWord m bot ws
renderDPWord m bot [] = error "can not render empty words"

renderWord m bot (w:[]) = translateBot m bot w
renderWord m bot (w:ws) = translateBot m bot w ++ "," ++ renderWord m bot ws
renderWord m bot [] = ""

renderS m bot steps sys = unlines $ (fmap (translateSt m bot sys) steps)

translateSt m bot sys (Step w p rv)  = 
    if isDPSystem sys
    then "<" ++ renderDPWord (swapMap m) bot (getWord w) ++ ">,"
         ++ "< " ++ maybeShowpos (getEnd w) ++ ">,"
         ++ "< " ++ maybeShowpos p ++ ">,"
         ++ " " ++ maybeShowrv rv
    else "<" ++ renderWord (swapMap m) bot (getWord w) ++ ">,"
         ++ "< " ++ maybeShowpos (getEnd w) ++ ">,"
         ++ "< " ++ maybeShowpos p ++ ">,"
         ++ " " ++ maybeShowrv rv

renderSteps :: (Show a1, Show a, Ord t) =>
     Map String t -> String -> [DeriveStep t a a1] -> String
renderSteps m bot steps = unlines $ (fmap (translateStep m bot) steps)

translateStep m bot (Step w p rv) = 
    (concatMap (translateBot (swapMap m) bot) (getWord w)) 
    ++ ", " ++ maybeShowpos (getEnd w)
    ++ ", " ++ maybeShowpos p
    ++ ", " ++ maybeShowrv rv

maybeShowpos p = case p of
    Just p' -> show p' ++ " "
    Nothing -> "- "

maybeShowrv rv = case rv of
    Just r  -> show $ M.toList r 
    Nothing -> "- "

translateBot m q s = case M.lookup s m of
    Just d  -> d
    Nothing -> q

swapMap :: (Ord k, Ord v) => Map k v -> Map v k
swapMap = M.fromList . (fmap swap) . M.toAscList where
    swap (a,b) = (b,a) 
