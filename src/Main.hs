{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char (toLower)
import Data.Map (fromList, (!), Map)
import Data.Typeable
import DGG.Adapter
import DGG.Adapter.EMGM
import DGG.Adapter.MultiRec
import DGG.Adapter.SYB
import DGG.Data
import DGG.Parser
import Language.Haskell.Exts
import System.Console.CmdArgs

data DGGArgs = DGGArgs { adapter  :: String
                       , input    :: String
                       , datatype :: String
                       , output   :: String
                       }
                       deriving (Show, Data, Typeable)

dgg :: DGGArgs
dgg = DGGArgs { adapter  = def &= help "Adapter name. E.g.: EMGM"
              , input    = def &= help "Input file"
              , datatype = def &= help "Specify datatype for which to derive. Generates for all datatypes if left blank."
              , output   = def &= help "Output file. E.g.: Instances.hs"
              }
              &= summary "DGG: Datatype Generic Generator v0.1-dev"

main :: IO ()
main = do
    args <- cmdArgs dgg
    -- TODO: This is rather ugly. Can't CmdArgs do this kind of thing?
--    if (null $ adapter dgg) || (null $ input dgg)
--        then error "Specify at least an adapter and input file."
--        else return ()
    pr   <- parseFile (input args)
    adap <- return (map toLower $ adapter args)
    code <- return $ genCode pr (adapters ! adap) (supports ! adap) (imports ! adap)
    if hasFileOutput args
        then writeFile (output args) code
        else putStrLn code
    
hasFileOutput :: DGGArgs -> Bool
hasFileOutput = not . null . output

-- TODO: This is all hardcoded now. Perhaps this could be done more nicely?
-- Do these three maps need to be combined using a tuple ?
adapters :: Map String LibParser
adapters = fromList [ ("emgm",     makeEMGM)
                    , ("syb",      makeSYB)
                    , ("multirec", makeMultiRec) ]

supports :: Map String (Decl -> Bool)
supports = fromList [ ("emgm",     isSuppEMGM)
                    , ("syb",      isSuppSYB)
                    , ("multirec", isSuppMultiRec) ]

imports :: Map String [ImportDecl]
imports = fromList [ ("emgm",     importsEMGM)
                   , ("syb",      importsSYB)
                   , ("multirec", importsMultiRec) ]

