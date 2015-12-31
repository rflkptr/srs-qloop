import CLI.CLI    as C
import SRS.System as S

import Encode.SAT as E
import Encode.QSAT as Q
import System.FilePath
import System.Directory
import qualified Data.ByteString.Lazy as BS

run :: C.Options -> IO ()
run (C.Options Nothing outFile encoding flags cmd) = do
    srs' <- getContents
    res  <- readinput srs'
    case res of
        Just (SRS []) -> error "parsed empty SRS"
        Just srs'     -> if C.dp_transform flags
            then runCMD (toDPSystem srs') outFile encoding flags cmd
            else runCMD srs' outFile encoding flags cmd
        Nothing       -> print "Cannot parse input to string rewriting system."

run (C.Options (Just filePath) outFile encoding flags cmd) = do
    ex <- doesFileExist filePath
    if ex 
        then do
            srs <- S.readSRS filePath
            -- here goes DP transformation
            case srs of
                Just (SRS []) -> error "parsed empty SRS"
                Just srs'     -> if C.dp_transform flags
                    then runCMD (toDPSystem srs') outFile encoding flags cmd
                    else runCMD srs' outFile encoding flags cmd
                Nothing       -> print "Cannot parse file to string rewriting system."
        else putStrLn "File does not exist"

runCMD srs (Just outFile) encoding flags (SAT options) = do
    BS.writeFile outFile $ E.getEncodingBS srs encoding flags options
    if (C.solveInstance flags)
        then E.solve srs encoding flags options
        else putStrLn $ show srs

runCMD srs Nothing encoding flags (SAT options) = do
    if (C.solveInstance flags)
        then E.solve srs encoding flags options
        else putStrLn $ show srs

runCMD srs (Just outFile) encoding flags (QSAT options) = do
    BS.writeFile outFile $ Q.qgetEncodingBS srs encoding flags options
    if (C.solveInstance flags)
        then Q.qsolve srs encoding flags options
        else putStrLn $ show srs

runCMD srs Nothing encoding flags (QSAT options) = do
    if (C.solveInstance flags)
        then Q.qsolve srs encoding flags options
        else putStrLn $ show srs

main :: IO ()
main = C.execCLI >>= run
