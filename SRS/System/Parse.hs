module SRS.System.Parse
( readSRS
, readinput
) where

import SRS.System.Type
import SRS.Rule.Type

import qualified TPDB.XTC as TX
import qualified TPDB.Convert as TC
import qualified TPDB.Plain.Read as TR
import qualified TPDB.Data as TD
import Data.Char (chr)
import Text.Parsec.Prim
import Text.XML.HXT.Arrow.XmlState
import Text.XML.HXT.Arrow.ReadDocument ( readString )
import Control.Arrow ((>>>))

--readSRS :: String -> IO (Maybe (System [] String))
readSRS :: FilePath -> IO (Maybe (System [String]))
readSRS inFile = do
    s <- readFile inFile
    case TR.srs s of
        Right (TD.RS s r b) -> case to_srs r of
            SRS (l:ls) -> return $ Just (SRS (l:ls))
            _          -> xmlP inFile
        _                   -> xmlP inFile

xmlP :: FilePath -> IO (Maybe (System [String]))
xmlP inFile = do
    [p] <- TX.readProblems inFile
    case TC.trs2srs (TX.trs p) of
        Just (TD.RS s' r b') -> case to_srs r of
            SRS (l:ls) -> return $ Just (SRS (l:ls))
            _          -> return $ Nothing
        Nothing               -> return $ Nothing

--readinput :: String -> IO (Maybe (System [] String))
readinput :: String -> IO (Maybe (System [String]))
readinput userInput = 
    case TR.srs userInput of
        Right (TD.RS s r b) -> case to_srs r of
            SRS (l:ls) -> return $ Just (SRS (l:ls))
            _          -> tryXML userInput
        _                   -> tryXML userInput

tryXML :: String -> IO (Maybe (System [String]))
tryXML userInput = do
    [p] <- readXMLInput userInput
    case TC.trs2srs (TX.trs p) of
        Just (TD.RS s' r b')    -> case to_srs r of
            SRS (l:ls) -> return $ Just (SRS (l:ls))
            _          -> return $ Nothing
        Nothing                 -> return $ Nothing
        
readXMLInput input = runX (readString [] input >>> TX.getProblem)

to_srs = SRS . fromTRules

fromTRules = Prelude.map (fromTRule)

fromTRule :: TD.Rule [TD.Identifier] -> Rule [String]
fromTRule (TD.Rule l r _ _) = rule_rep l r

rule_rep :: [TD.Identifier] -> [TD.Identifier] -> Rule [String]
rule_rep l r = Rule (Prelude.map (TD.name) l) (Prelude.map (TD.name) r)
