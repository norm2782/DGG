{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Typeable
import System.Console.CmdArgs

data DGGArgs = DGGArgs { adapter  :: String
                       , datatype :: String
                       , foutput  :: String
                       }
                       deriving (Show, Data, Typeable)

dgg :: DGGArgs
dgg = DGGArgs { adapter  = def &= help "Adapter name. E.g.: EMGM"
              , datatype = def &= help "Specify datatype for which to derive. Generates for all datatypes if left blank."
              , foutput  = def &= help "Output file. E.g.: Instances.hs"
              }
              &= summary "DGG: Datatype Generic Generator v0.1"

main :: IO ()
main = do
    args <- cmdArgs dgg
    print args

hasFileOutput :: DGGArgs -> Bool
hasFileOutput dgg = not $ null $ foutput dgg
