{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char (toLower)
import Data.Map (fromList, (!), Map)
import Data.Typeable
import DGG.Adapter
import DGG.Data
import DGG.Parser
import Language.Haskell.Exts
import System.Console.CmdArgs

data DGGArgs = DGGArgs { adapter  :: String
                       , finput   :: String
                       , datatype :: String
                       , foutput  :: String
                       }
                       deriving (Show, Data, Typeable)

dgg :: DGGArgs
dgg = DGGArgs { adapter  = def &= help "Adapter name. E.g.: EMGM"
              , finput   = def &= help "Input file"
              , datatype = def &= help "Specify datatype for which to derive. Generates for all datatypes if left blank."
              , foutput  = def &= help "Output file. E.g.: Instances.hs"
              }
              &= summary "DGG: Datatype Generic Generator v0.1"

main :: IO ()
main = do
    args <- cmdArgs dgg
    pr   <- parseFile (finput args)
    adap <- return (map toLower $ adapter args)
    code <- return $ genCode pr (adapters ! adap) (supports ! adap)
    if hasFileOutput args
        then writeFile (foutput args) code
        else putStrLn code
    
hasFileOutput :: DGGArgs -> Bool
hasFileOutput = not . null . foutput

-- TODO: This is all hardcoded now. Perhaps this could be done more nicely?
-- Do these two maps need to be combined using a tuple ?
adapters :: Map String LibParser
adapters = fromList [ ("emgm",     makeEMGM)
                    , ("syb",      makeSYB)
                    , ("multirec", makeMultiRec) ]

supports :: Map String (Decl -> Bool)
supports = fromList [ ("emgm",     isSuppEMGM)
                    , ("syb",      isSuppSYB)
                    , ("multirec", isSuppMultiRec) ]
