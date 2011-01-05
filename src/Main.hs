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

data Adapter = Adapter { makeFn    :: LibParser
                       , isSuppFn  :: (Decl -> Bool)
                       , importsFn :: [ImportDecl] }

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
    adap <- return $ adapters ! (map toLower $ adapter args)
    code <- return $ genCode pr (makeFn adap) (isSuppFn adap) (importsFn adap)
    if hasFileOutput args
        then writeFile (output args) code
        else putStrLn code
    
hasFileOutput :: DGGArgs -> Bool
hasFileOutput = not . null . output

adapters :: Map String Adapter
adapters = fromList [ ("emgm",     Adapter makeEMGM     isSuppEMGM     importsEMGM)
                    , ("syb",      Adapter makeSYB      isSuppSYB      importsSYB)
                    , ("multirec", Adapter makeMultiRec isSuppMultiRec importsMultiRec) ]
