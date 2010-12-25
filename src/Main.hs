{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Typeable
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
    code <- return $ genCode $ finput args
    if hasFileOutput args
        then writeFile (foutput args) code
        else putStrLn code
    
hasFileOutput :: DGGArgs -> Bool
hasFileOutput dgg = not $ null $ foutput dgg

genCode :: String -> String
genCode = id
