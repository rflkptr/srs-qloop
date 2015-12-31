module CLI.CLI
( Options (..)
, Command (..)
, Flags (..)
, QSATOptions (..)
, SATOptions (..)
, Dimension (..)
, Encoding (..)
, execCLI
) where

import Options.Applicative as O
import System.FilePath

opts :: ParserInfo Options
opts = info (helper <*> parseOptions)
    ( fullDesc
     <> progDesc "Program to qbf/sat encode looping properties of string rewriting systems."
     <> header "QENCODE" )

execCLI :: IO Options
execCLI = execParser opts

data Options = Options 
    { input_file  :: Maybe FilePath
    , output_file :: Maybe FilePath
    , encoding    :: Encoding
    , flags       :: Flags
    , cmd         :: Command
    } deriving Show

parseOptions = Options
    <$> optional (
            strOption
                ( long "input-file"
                    <> short 'i'
                    <> metavar "FILE"
                    <> help "Input file which contains the string rewriting system")
        )
    <*> optional (
            strOption 
                ( long "output-file"
                    <> short 'o'
                    <> metavar "FILE"
                    <> help "Output file to which the encoded problem instance gets written to.")
        )
    <*> parseEncoding
    <*> parseFlags
    <*> parseCMD

data Encoding = Binary | Onehot | Order deriving (Show, Eq)

parseBinary = flag Binary Binary
    ( long "binary"
    <> help "Encode symbols binary (Default)" )

parseOnehot = flag Binary Onehot
    ( long "onehot"
    <> help "Encode symbols onehot" )

parseOrder = flag Binary Order
    ( long "order"
    <> help "Encode symbols order" )


parseEncoding = parseBinary
            <|> parseOnehot
            <|> parseOrder

data Flags = Flags 
    { dp_transform  :: Bool
--    , abstract_pos  :: Bool
    , check_LP      :: Bool
    , solveInstance :: Bool
    } deriving Show

parseFlags = Flags
    <$> switch
        ( long "dp-transform"
            <> short 'd'
            <> help "Calculate and encode the dependency pair problem according to the input SRS." )
--    <*> switch
--        ( long "abstract-positions"
--            <> short 'a'
--            <> help "Encode positions to be abstract. Formulas get bigger but the runtime of solvers can be improved." )
    <*> switch
        ( long "check-length-preserving"
            <> short 'c'
            <> help "Check if the system is length preserving in every rule. No word ends will be encoded." )
    <*> switch 
        ( long "solve"
            <> short 's'
            <> help "Invoke a solver and try to print a result on STDOUT" )

data Command = QSAT QSATOptions | SAT SATOptions deriving Show

parseCMD = 
    hsubparser $ 
        (  command "qsat" (info (QSAT <$> parserQSATOpt) (progDesc "encode as qsat instance"))
        <> command "sat"  (info (SAT  <$> parserSATOpt )  (progDesc "encode as sat instance"))
        )

data QSATOptions = QSATOptions 
    { quantorPairs :: Integer 
    , qdim         :: Dimension
    } deriving Show

parserQSATOpt :: O.Parser QSATOptions
parserQSATOpt = QSATOptions
    <$> O.option auto (long "all-quantor-count" <> short 'q' <> metavar "Integer" 
        <> help "How many all quantors will be used. (0 = SAT encoding)" )
    <*> parseDimension


data SATOptions = SATOptions 
    { sdim :: Dimension
    } deriving Show

parserSATOpt :: O.Parser SATOptions
parserSATOpt = SATOptions
    <$> parseDimension

data Dimension = Dimension
    { derivationSteps  :: Integer
    , wordLength       :: Integer
    } deriving Show

parseDimension = Dimension
    <$> O.option auto (long "max-loop-len" <> short 'l' <> metavar "Integer" 
        <> help "Maximum length of a looping derivation. Example: -l 3 means 3 steps in sat and 2^3 in qsat encodings." )
    <*> O.option auto (long "max-word-len" <> short 'w' <> metavar "Integer" 
                <> help "Maximum length of a word." )


