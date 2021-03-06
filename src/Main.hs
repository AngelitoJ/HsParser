
-- HsParser: A Parsec builder, a toy for experimenting things:
-- 1) Generic parser to explore automated parser generation
-- 2) A Email parsing tool
-- 3) Quantum Chemistry Basis sets parsing... (The Haskell HartreeFock Project )

-- @2013 Angel Alvarez, Felipe Zapata, from The ResMol Group  

module Main where

import Data.Maybe ( fromMaybe )
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad.Trans.Either
import System.Environment ( getArgs )
import System.FilePath
import System.IO
import System.Cmd ( system )
import System.Console.GetOpt

-- Cabal imports
import Data.Version (showVersion)
import Distribution.Version
import Paths_HsParser as HsParser

import OptsCheck
import Tasks
import GenericParser
import BasisParser
import MolcasParser

-- import FastParser


program = "Universal Parser"
authors = "@2013 Angel Alvarez, Felipe Zapata"

-- default options
defaultOptions    = Options
 { optDump        = False
 , optModules     = [("generic",processGenericFiles),("basis",processBasisFiles),("molcas",processMolcasFiles)]
 , optMode        = Nothing
 , optVerbose     = False
 , optShowVersion = False
 , optOutput      = Nothing
 , optDataDir     = Nothing
 , optInput       = []
 }

-- currently supported options
acceptedOptions :: [OptsPolicy]
acceptedOptions =
 [ 
   Option ['h','?'] ["help"]    (NoArg  ( check_help           ))                "Show this help message."
 , Option ['v']     ["verbose"] (NoArg  ( check_verbosity      ))                "Verbose run on stderr"
 , Option ['V']     ["Version"] (NoArg  ( check_version        ))                "Show version number"
 , Option ['D']     ["datadir"] (ReqArg ( check_data_dir       ) "Dir")          "Directory where files are located"
 , Option ['m']     ["mode"]    (ReqArg ( check_operation_mode ) "Mode")         "Mode of Operation"
 , Option []        ["dump"]    (NoArg  ( check_dump_options   ))                "Force args cmdline dump"
 ]
--    Option ['e']     ["error"]   (NoArg (\ _opts -> return $ Left "forced error on args detected!"))  "Force args checking error"
--  , Option ['i']     ["input"]   (OptArg (\f opts -> check_input_file f opts) "FILE")             "Input file"


main :: IO ()
main = do
    args   <- getArgs
    cores  <- getNumCapabilities
    progHeader cores
    result <- runEitherT $ progOpts args defaultOptions acceptedOptions
    either somethingIsWrong doSomeStuff result


somethingIsWrong :: String -> IO ()    
somethingIsWrong msg = do
             putStrLn $ "\nError: " ++ msg ++ "\n"
             putStrLn $ usageInfo header acceptedOptions

doSomeStuff :: Options -> IO ()
doSomeStuff optsR@Options { optMode = mode } = do
    case mode of
         Nothing -> printFiles optsR
         Just fun -> fun optsR

-- Keep calm and curry on, we are the good guys....
progHeader :: Int -> IO ()
progHeader c = 
    putStrLn $ program ++ " V:" ++ currVersion ++ " " ++ authors ++ "\n\t" ++ show(c) ++ " processor " ++ (core2string c) ++ " detected."
    where
        currVersion :: String
        currVersion = showVersion HsParser.version
        core2string :: Int -> String
        core2string c = case c > 1 of
                             True -> "cores"
                             False -> "core"

header :: String
header = "Usage: Options [OPTION...] files..."

-- | "Efects for dummies", this functions has no purpouses other than printng args
printFiles :: Options -> IO ()
printFiles opts@Options { optInput = files, optDataDir = datadir } = do
    mapM_ printargs filepaths 
    where
            dir = fromMaybe "" datadir
            filepaths = zipWith (combine) (cycle [dir]) files
            printargs :: String -> IO ()
            printargs path = putStrLn $ "Processing path: " ++ path ++ "..."

processGenericFiles :: Options -> IO ()
processGenericFiles opts@Options { optInput = files, optDataDir = datadir } = do
    mapM_ processGenericFile filepaths 
    where
            dir = fromMaybe "" datadir
            filepaths = zipWith (combine) (cycle [dir]) files

processBasisFiles :: Options -> IO ()
processBasisFiles opts@Options { optInput = files, optDataDir = datadir } = do
    mapM_ processBasisFile filepaths 
    where
            dir = fromMaybe "" datadir
            filepaths = zipWith (combine) (cycle [dir]) files

processMolcasFiles :: Options -> IO ()
processMolcasFiles opts@Options { optInput = files, optDataDir = datadir } = do
    mapM_ processMolcasOutputFile filepaths 
    where
            dir = fromMaybe "" datadir
            filepaths = zipWith (combine) (cycle [dir]) files


