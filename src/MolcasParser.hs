-- ------------------------------------------------------------------------------------
-- Molcas 2010 output file parser
-- @2013 The ResMol Group
-- Authors: Angel Alvarez, Felipe Zapata
--
--  2013-01 Initial design
--  2013-05 Fixed: mainly related to numeric routines
--  2013-10 Fixed: parsing output files with loops (dynamics) where Alaksa kicks in a few times...

-- Molcas:
-- Another bunch of ancient and (somewhat) heroic Fortran routines dating back from the 
-- mid-eighties to nowadays, yet a bit more structured that Gaus** routines that look 
-- even older... Both share the batch model of execution where multiple programs run in 
-- sequence to acomplish some calculation and (to some extent) they exercise common routines.
-- 
-- The output file:
-- The format seems a bit chaotic and poorly suited for machine parsing, yet lot of people 
-- here try to make a life upon this messy outputs so it deserved a good parser.

-- As far as we know the are a few main blocks "Copyright", logo", "project info", and some more
-- ending with "the auto module" that rules all the calculations workflow and embodies output
-- coming from the other modules "gateway", "seward", "alaska" and a few others...
-- 
-- YMMV comes to be the norm when expecting where data will be and why, actually some info 
-- moves back and forth between runs even to unexpected places, upon many diferent facts
-- not always related to alternative code branchs. the module auto kicks-in sometimes and you
-- have to get along with unexpected messages popping up at unexpected places...

-- We try to parse and convert unformatted text data into more manageable chunks with gracefull degradation
-- 1) Currently interesting modules data structures get stored using tuples and primtive types.
-- 2) Auxiliary data from those modules gets into a textual records where we store data we havent work it out yet.
-- 3) uninteresting modules get into text chunks out of text lines, yet we try to record where we found it 
-- As soon as a project needs new data we make proper parsing combinators and types to support it.

-- Such a "mess" makes you feel shocked that this programming culture achieves so many things. 
-- Even as is, this is a piece of impressive knowledge and hard work, we honor that. So please take 
-- no ofense whose who dare to dig into the code. 

-- Hopefully this work will somehow allow people to do easy data parsing of this gigant in chemistry.
--
--                     ....We promise, no chemies were harmed during this development.


-- As of 2013/10 current parsed text chucks include:
-- Main blocks: 
--          Logo, CopyRight, Project, and the "Auto" module
-- Lower modules:
--          Alaska/Molecular gradients sections
--          RASSCF/Final state energies
-- See types and dtat section for further details

-- ------------------------------------------------------------------------------------

module MolcasParser where

import Data.Char
import qualified Data.List.Split as DLS
import Text.Parsec
import Text.Parsec.String

import ParsecCommon
import ParsecNumbers
import ParsecText


type Name = String

type BlockParser   = MyParser MolState MolBlock
type SectionParser = MyParser MolState SectionData 

-- Pretty simple types right now, but will have type parameters in the near future
-- Molblocks represent main Molcas format blocks including the auto module as a whole
data MolBlock =
      LicenseData String                                     -- LicenseData contains license information 
    | LogoData    String                                     -- LogoData contains version info 
    | CopyRightData String                                   -- Copy... contains law related things
    | InputData  String                                      -- Original input file text used for this run
    | ProjectData String String String String                -- Project details, submission, scratch dir etc...
    | ModuleAuto Name String [ModuleData]                    -- Molcas auto module and all moules it ran
    | TextData    String                                     -- just unformatted text as we got it 

-- Most Molcas modules run from the auto module since the latter control flow of execution
data ModuleData = 
     ModuleData Name String [SectionData]                    -- Molcas modules data stored in sections 

-- Every molcas module produces some data results, some of them have diferenciated types for better exploitation...
data SectionData = 
      WhateverData String                                    -- Data not really parsed and left as unformatted data
    | GatewayData String
    | SewardData String
    | RasSCFData String                                      -- RASSCF unformatted output
    | RasSCFCI Int Double [(Int, String, Double, Double)]    -- RASSCF CI Roots info
    | RasSCFRE [(Int,Double)]                                -- RASSCF Energies
    | AlaskaData String                                      -- Alaska unformatted output
    | AlaskaMolecularGradients String [(String,Char,Double)] -- Alaska Molecular Gradients

-- Common state record to all Molcas parsers
data MolState = MolState 
    { 
          blockLevel   :: [BlockParser]                       -- Block level Molcas blocks
        , sectionLevel :: [SectionParser]                     -- Section Level Molcas Blocks
    }


instance Show MolBlock where
    show (TextData s)             = "Text    : " ++ s
    show (LicenseData s)          = "License : " ++ s
    show (LogoData s)             = "Logo    : " ++ s
    show (CopyRightData s)        = "Copy Right : " ++ s
    show (InputData s)            = "Input   : " ++ s
    show (ProjectData p f s x)    = "Project info:\n\tName: " ++ p ++ "\n\tSubmitted: " ++ f ++ "\n\tScratch: " ++ s ++ "\n\tOuputs: " ++ x
    show (ModuleAuto n s modules) = "Module : " ++ n ++ " " ++ s ++ "\n" ++ concat (map show modules)

instance Show ModuleData where
    show (ModuleData n s items)   = "Module : " ++ n ++ " " ++ s ++ "\n" ++ concat (map show items)
    
instance Show SectionData where
    show (WhateverData s)                   = "Module Data: " ++ (show s) ++ "\n"
    show (RasSCFData s)                     = "RasSCF Data: " ++ (show s) ++ "\n"
    show (RasSCFCI r e ci)                  = "RasSCF CI-Coefficients for root: " ++ (show r) ++ "\nEnergy: " ++ (show e) ++ "\ninfo:" ++ (show ci) ++ "\n"
    show (RasSCFRE roots)                   = "RasSCF Final Energies:" ++ (show roots)++"\n"
    show (AlaskaData s)                     = "Alaska Data: " ++ (show s) ++ "\n"
    show (AlaskaMolecularGradients r grads) = "Alaska Molecular Gradients [" ++ r ++ "]:" ++ (showMolGrads grads) ++ "\n"



-- Public Interface ----------------------------------------------------------------------------------


-- Create a defualt Molcas Parser state 
defaultParserState :: MolState 
defaultParserState = MolState {
                              blockLevel   = []  
                            , sectionLevel = []    
                        }

-- Parse a given file path containing a ouput Molcas file and display it
processMolcasOutputFile :: FilePath -> IO ()
processMolcasOutputFile fname = do
        putStrLn $ "Processing file:" ++ (show fname) ++ "\n"
        input  <- readFile fname
        case (runParser molcasOutParser defaultParserState fname input) of
             Left err  -> print err
             Right xs  -> mapM_ print xs

-- Parse a given file path pointing to a Molcas output file 
parseMolcasOutputFile :: FilePath -> IO (Either ParseError [MolBlock])
parseMolcasOutputFile fname = do
        input  <- readFile fname
        return $ runParser molcasOutParser defaultParserState fname input

-- ------------------------------------------------------------------------------------------------------------
--  Top level parsers, one to input files (just a hack), one to Molcas output files 
-- ------------------------------------------------------------------------------------------------------------

-- Quick Hack to parse the Molcas Input file format and gather a parameter
molcasInpParser :: MyParser MolState (String,Int,String)
molcasInpParser = do
    firstPart <- manyTill anyChar (try (string "SALA"))
    newline
    spaces
    root <- intNumber
    spaces
    lastPart <- manyTill anyChar (try eof)
    return $ (firstPart,root,lastPart)

-- Parsec parser for Molcas Output format
molcasOutParser :: MyParser MolState [MolBlock]
molcasOutParser = do
    modifyState (\x -> x { blockLevel = parsers } ) -- Set the Block parsers according to main part of a Molcas file
    elements <- many1 molcasBlock                   -- Get at least one of the known Molcas Blocks
    return $ elements
    where
        parsers = [ 
                    molcasBlank                       -- Who needs blanks? we dont
                    , molcasLicense                   -- Is this a Molcas license block?
                    , molcasLogo                      -- or the Molcas "Logo"?
                    , molcasCopyRight                 -- or who rules the world?
                    , molcasProject                   -- or project data?
                    , molcasAuto                      -- the auto module
                    , molcasWhatever                  -- You guess what? Actually I just dont care
                    ]

-- ------------------------------------------------------------------------------------------------------------
--  Molcas Output Block level parsers:
--  one to every know main block of a output Molcas file, ie: License,Project info and so on
-- ------------------------------------------------------------------------------------------------------------


-- Parse a Molcas block from the (user state provided) list of known block level parsers 
molcasBlock :: BlockParser
molcasBlock = do
    userState <- getState                 -- get block level parsers from the user state
    choice $ blockLevel userState           -- try the top level parsers
 
-- Try to parse as many blank text lines as it can, and collapse them into a single TextData MolBlock 
molcasBlank :: BlockParser
molcasBlank = try $ do
    many1 newline                   -- take easy man, we dont judge what we find...
    return $ TextData "[BLANK]"

-- Parse while text lines into MolBlocks 
molcasWhatever :: BlockParser
molcasWhatever = do
    iFound <- anyLine                   -- take easy man, we dont judge what we find...
    return $ TextData iFound

-- Parse Molcas License Info
molcasLicense :: BlockParser
molcasLicense = try $ do
    string "   License is going to expire in "
    days      <- intNumber
    string " days  on "
    date      <- manyTill anyChar newline
    string "   This copy of MOLCAS is licensed to "
    user      <- manyTill anyChar newline
    let licenseData = "Expires in " ++ date ++ " (" ++ (show days) ++ "), Licensed to: " ++ user
    return $ LicenseData licenseData

-- Parse Molcas Logo 
molcasLogo :: BlockParser
molcasLogo = try $ do
    spaces
    string "^^^^^"
    spaces
    string "M O L C A S"
    spaces
    spaces
    string "^^^^^^^"
    spaces
    string "version"
    space
    version       <- versionString -- 7.6
    string " patchlevel "
    patch         <- manyTill digit space
    spaces
    count 35 anyLine              -- its a big logo!
    let logoData = "Version " ++ version ++ " patchlevel " ++ patch
    return $ LogoData logoData    -- return as single string

-- Try parse a Molcas copyright text block, return only university info and year
molcasCopyRight:: BlockParser
molcasCopyRight = try $ do
    stringLine "Copyright, all rights, reserved:"
    stringLine "Permission is hereby granted to use"
    stringLine "but not to reproduce or distribute any part of this"
    stringLine "program. The use is restricted to research purposes only."
    message <- stringLine "Lund University Sweden, 2010."
    stringLine "For the author list and the recommended citation consult section 1.5 of the MOLCAS user's guide."
    return $ CopyRightData message

    
-- Try parse the input file data from the auto module ouput
molcasInput :: BlockParser
molcasInput = try $ do
    string "++ ---------   Input file   ---------"
    contents <- manyTill anyLine (try (string "-- ----------------------------------"))
    newline
    return $ InputData (concat contents)

--  Try to parse Project data returning a proper ProjectData block
molcasProject:: BlockParser
molcasProject = try $ do
    string "   -------------------------------------------------------------------"
    newline
    string "  |                                                                   "
    newline
    string "  |   Project         = "
    name                   <- manyTill anyChar (try newline) -- "gradR4"
    string "  |   Submitted from  = "
    submitted              <- manyTill anyChar (try newline) -- "/scratch/cris/gradR4.3173"
    string "  |   Scratch area    = "
    scratch                <- manyTill anyChar (try newline) -- "/scratch/cris/gradR4.3173/"
    string "  |   Save outputs to = "
    output                 <- manyTill anyChar (try newline) -- "/home/cris/MOL_MOT/luisma_iminium_out/conform/conf_S0_dul/gradientes/R4"
    string "  |                                                                   "
    newline
    string "  |   Scratch area is NOT empty"
    newline
    string "  |                                                                   "
    newline
    string "  |                                                                   "
    newline
    string "   -------------------------------------------------------------------"
    newline
    return $ ProjectData name submitted scratch output

-- Try to parse the auto module output, store runtime statistics and include output inside module runs
molcasAuto :: BlockParser
molcasAuto = try $ do
    (name, whenStart)             <- moduleStart                           -- The module started
    blankLine
    input                         <- molcasInput                           -- A copy of input file is provided
    blankLine
    workdir                       <- option "" workDir                     -- One or more workdir sentences can appear here (and there)
    modules                       <- many1 molcasModule                    -- A bunch of modules, come ahead...
    run                           <- autoStatus                            -- Dont look at me!, im just parsing your results...
    (name1, whenStop,returnCode)  <- moduleStop                            -- The module stopped, hopefully
    duration                      <- option "" $ moduleSpent name
    return $ ModuleAuto name (whenStart ++ " " ++ whenStop ++ " " ++ duration ++ " RC: " ++ returnCode) modules


-- Parse a normal (non auto) Molcas Module, and check for suitable sections upon checking module name
-- This parsing is crazy, Molcas modules seem to have duplicate branchs of code printing out the same
-- info yet not really the same...
molcasModule :: MyParser MolState ModuleData
molcasModule = try $ do
    (name, whenStart)                 <- moduleStart                    -- The module started
    items                             <- manyTill (sectionParser name) $ lookAhead $ try moduleStop 
    (moduleName, endTime, returnCode) <- moduleStop
    duration                          <- option "" $ moduleSpent name   -- sometimes it just pops up, sometimes not 
    workdirReset                      <- option "" workDir              -- Who is messing with the work dir, uhh?
    autospent                         <- option "" $ moduleSpent "auto" -- "Shit happens" - Forrest Gump
    return $ ModuleData name ("Started: " ++ whenStart ++ " Stopped (" ++ returnCode ++ ") " ++ endTime ++ " " ++ duration ) items

-- "--- Start Module: seward at Sun Dec  2 22:41:44 2012"
moduleStart :: MyParser MolState (String,String)
moduleStart = do
    optional ( try (string "*** " >> newline))                       -- Depends upon weather on Sweden, the module and the daily fellow 
    string "--- Start Module:"                                       -- Here we go, fasten your seat belts
    skipMany space
    moduleName <- many1 alphaNum
    skipMany space 
    string "at"
    content <- anyLine
    return $ (moduleName, content )

-- "--- Stop Module:  gateway at Sun Dec  2 22:41:43 2012 /rc=0 ---" 
moduleStop :: MyParser MolState (String, String, String)
moduleStop = do
    string "--- Stop Module:"
    skipMany space
    moduleName <- many1 alphaNum
    skipMany space 
    string "at"
    endTime  <- manyTill anyChar (string "/rc=")
    returnCode <- manyTill anyChar (string "---" >> newline)
    return $ (moduleName, endTime, returnCode)

-- Try to guess how this run of Molcas finished, return a (somewhat) descriptive message
autoStatus :: MyParser MolState String
autoStatus = do
    happyLanding 
    <|> convergenceProblem                                            -- You fool, that's pretty big for us
    <|> unknownProblem                                                -- Cosmic Rays
    <?> "Really hard to guess this!"
    where
        happyLanding :: MyParser MolState String
        happyLanding = try $ do
            blankLine
            string "     Happy landing!"                               -- Normal case you are lucky such a beast didnt kill your cat 
            newline
            blankLine
            return "Happy Landing"
        convergenceProblem :: MyParser MolState String
        convergenceProblem = try $ do
            string "          "
            newline
            string "    Convergence problem!"                          -- Error Convergence problem, whatever that means 
            newline
            string "          "
            newline
            count 8 anyLine                                            -- Molcas Gurus wont tell you, man you know you did wrong..
            string "Non-zero return code - check program input/output" -- Clever clue
            newline
            return "Convergence Problem"
        unknownProblem :: MyParser MolState String
        unknownProblem = try $ do
            many1 anyLine                                              -- Who knows, just ask Roland...
            return "Unknown Problem"

-- Parse WorkDir rset line
-- This message  is know to appear in many unrelated parts of the output file...
workDir :: MyParser MolState String
workDir = do
    lines <- many1 (string "WorkDir " >> anyLine)
    return $ head lines

-- Parse a simple version string as [0-9]'.'[0-9]
versionString :: MyParser MolState String
versionString = do
    major <- digit
    char '.'
    minor <- digit
    let version = major : '.' : [minor]
    return $ version
    

moduleSpent :: String -> MyParser MolState String
moduleSpent name = try $ do
    string "--- Module "
    string name
    string " spent "
    spent <- anyLine
    return spent

-- ------------------------------------------------------------------------------------------------------------
--  Molcas Output Section level parsers:
--  one to every know sections from modules data, i.e. Alaska Molecular Gradients or Rasscf Energies
-- ------------------------------------------------------------------------------------------------------------

sectionParser :: String -> SectionParser
sectionParser name = case name of
    "alaska" -> do
                sectionMolecularGradients                  -- Molecular Gradient printout
                <|> sectionsWhatever name                  -- rest of Alaska sections
                <?> "alaska me cachis!"                    -- You know what I mean, man
    "rasscf" -> do
                sectionCiCoefficients                      -- rasSCF Wave function printout (occupation of active orbitals, and spin coupling of open shells )
                <|> sectionRASSCFEnergies                  -- Final rasSCF Energies
                <|> sectionsWhatever name                  -- rest of RASSCF sections
                <?> "rasscf me cachis!"                    -- Should we phone Roland one more time?
    other    -> do
                sectionsWhatever other                     -- We dont have a section parser for this module sections...

-- Parse sections lines into a suitable generic ModuleData 
sectionsWhatever :: String -> SectionParser
sectionsWhatever "alaska" = do
    iFound <- anyLine
    return $ AlaskaData iFound
sectionsWhatever "rasscf" = do
    iFound <- anyLine
    return $ RasSCFData iFound
sectionsWhatever name = do
    iFound <- anyLine
    return $ WhateverData $ "[" ++ name ++ "] " ++ iFound    -- Well, I dont know what is it


-- Try to parse Molecular gradients from alaska Molcas modules
sectionMolecularGradients :: SectionParser
sectionMolecularGradients = try $ do
    string " **************************************************"
    newline
    string " *                                                *"
    newline
    string " *              Molecular gradients               *"
    newline
    string " *                                                *"
    newline
    string " **************************************************"
    newline
    blankLine
    string "                Irreducible representation:"
    spaces
    representation <- oneOfStrings ["a","s1"]
    spaces
    elements       <- many1 molecularGradient
    return $ AlaskaMolecularGradients representation elements


-- Try parse molecular gradients items from alaska
-- "                C1       x                 0.6259617E-01"
molecularGradient:: MyParser MolState (String,Char,Double)
molecularGradient = try $ do
    spaces
    atom           <- many1 alphaNum
    spaces
    coord          <- oneOf "xyz"
    spaces
    value          <- realNumber
    newline
    return $ (atom,coord,value)

-- Try parse Ci root data from rasscf Molcas module
sectionCiCoefficients :: SectionParser
sectionCiCoefficients = try $ do
    skipMany1 space
    string "printout of CI-coefficients larger than"
    skipMany1 space
    threshold <- realNumber                   --"0.05" 
    skipMany1 space
    string "for root"
    skipMany1 space
    root <- intNumber
    newline
    skipMany1 space
    string "energy="
    skipMany1 space
    energy <- realNumber
    newline
    skipMany1 space
    string "conf/sym"
    skipMany1 space
    mask <- manyTill (char '1') space -- 11111111
    skipMany1 space
    string "Coeff"
    skipMany1 space
    string "Weight"
    newline
    elements <- many1 coefficientElement
    return $ RasSCFCI root energy elements

sectionRASSCFEnergies :: SectionParser
sectionRASSCFEnergies = try $ do
    string "      Final state energy(ies):"
    newline
    string "      ------------------------"
    newline
    anyLine
    elements <- many1 rootEnergy
    return $ RasSCFRE elements

-- Parse rasscf root Energy
-- ::    RASSCF root number  2 Total energy =       -821.43755900                                                          
rootEnergy :: MyParser MolState (Int,Double)
rootEnergy = try $ do
    string "::    RASSCF root number"
    spaces
    root <- intNumber
    string " Total energy ="
    spaces
    energy <- realNumber
    anyLine
    return $ (root,energy)

-- Parse rasscf CI root info "    1  22220000  -0.91395 0.83531"
coefficientElement :: MyParser MolState (Int, String, Double, Double)
coefficientElement = try $ do
    skipMany1 space
    confsym <- intNumber
    skipMany1 space
    flags <- many1 alphaNum
    skipMany1 space
    coeff <- realNumber
    skipMany1 space
    weight <- realNumber
    newline
    return (confsym, flags, coeff, weight)

-- -------------------------------------------------------------------------------
-- 
-- Filtering combinators ans Show Instances 
-- 
-- -------------------------------------------------------------------------------

-- is this Block, the Molcas auto module?
isAuto :: MolBlock -> Bool
isAuto (ModuleAuto _n _s _modules) = True
isAuto _ = False

-- Test if given Module have a specified name
isModule :: String -> ModuleData -> Bool
isModule name (ModuleData n _s _sections) = n == name

--  Combine two module tests into one OR'ing the results of both test
orModule :: (ModuleData -> Bool) -> (ModuleData -> Bool) -> (ModuleData -> Bool)
orModule a b = \x -> (a x) || (b x)

infixr 2 `orModule`

-- Test for desired Module
isAlaska, isRASSCF :: ModuleData -> Bool
isAlaska = isModule "alaska"
isRASSCF = isModule "rasscf"

-- test for desired SectionData
isCICoeffs, isREnergies, isMolGrads :: SectionData -> Bool
isCICoeffs (RasSCFCI _r _e _ci) = True
isCICoeffs _ = False
isREnergies (RasSCFRE _roots) = True
isREnergies _ = False
isMolGrads (AlaskaMolecularGradients _r _grads) = True 
isMolGrads _ = False


-- -------------------------------------------------------------------------------
-- 
-- Display functions to use in Show instances 
-- 
-- -------------------------------------------------------------------------------


-- Make a Alaska Molecular Gradients listing suitable for humans...
showMolGrads :: [(String,Char,Double)] -> String
showMolGrads list = concat $ map showGrad $ chunkedlist
    where
        chunkedlist = DLS.chunksOf 3 list
        showGrad [(a1,x,vx),(a2,y,vy),(a3,z,vz)] = "\n\tAtom " ++ a1 
            ++ "  " ++ [x] ++ " = " ++ (show vx)
            ++ ", " ++ [y] ++ " = " ++ (show vy)
            ++ ", " ++ [z] ++ " = " ++ (show vz)



