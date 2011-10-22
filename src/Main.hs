{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Char
import Data.Generics
import Data.Map (fromList, (!), Map)
import Data.Typeable
import System.Console.CmdArgs

data DGGArgs = DGGArgs {
     adapter     :: String
  ,  inputfile   :: String
  ,  outputfile  :: String
  ,  modulename  :: String
  ,  identmap    :: String
  }
  deriving (Show, Data, Typeable)

dgg :: DGGArgs
dgg = DGGArgs {
     adapter     = def &= help "Adapter name. E.g.: EMGM"
  ,  inputfile   = def &= typFile &= help "Input file"
  ,  outputfile  = def &= typFile &= help "Output file"
  ,  modulename  = def &= help "Name of the module. Defaults to GenericReps."
  ,  identmap    = def &= typFile &= help "Identifier map"
  }
  &= summary "DGG: Datatype Generic Generator v0.2-dev"
  &= program "dgg"

main :: IO ()
main = do
    args <- cmdArgs dgg
    -- TODO: This is rather ugly. Can't CmdArgs do this kind of thing?
    when ((null . adapter) args || (null . inputfile) args) $
        error "Specify at least an adapter and an input file."
    let mn | null $ modulename args  = "GenericReps"
           | otherwise               = modulename args
    pr   <- parseFile (input args)
    let adap  = adapters ! map toLower (adapter args)
    let code  = genCode pr mn (makeFn adap) (isSuppFn adap) (importsFn adap)
    if (not . null . output) args
        then writeFile (output args) code
        else putStrLn code
