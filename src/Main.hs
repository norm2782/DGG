{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char (toLower)
import Data.Generics
import Data.Map (fromList, (!), Map)
import Data.Typeable
import DGG.Data
import DGG.Parser
import Language.Haskell.Exts
import System.Console.CmdArgs

-- TODO: This probably shouldn't be a string, but some datatype representing
-- the adapter. The adapter needs to be called in mkSrc.
-- Each of the adapters needs to have its own "main" function with a unique
-- name. A naming convention needs to be created for this. Initially, the 
-- easiest approach would be to just hardcode all supported adapters. Later,
-- dynamic adapter support could be investigated, possibly using variability
-- mechanisms.

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
    code <- return $ genCode pr (adapters ! (map toLower $ adapter args))
    if hasFileOutput args
        then writeFile (foutput args) code
        else putStrLn code
    
hasFileOutput :: DGGArgs -> Bool
hasFileOutput dgg = not $ null $ foutput dgg

genCode :: ParseResult Module -> LibParser -> String
genCode (ParseFailed l m) _ = error $ "Failed to parse module. Error on line " ++ show l ++ ": " ++ m 
genCode (ParseOk m)       p = prettyPrint (mkSrc p $ listify isSuppDecl m)

mkSrc :: LibParser -> [Decl] -> Decl
mkSrc a []     = undefined
mkSrc a (x:xs) = undefined

-- Returns True when the Decl is of the right type and False otherwise. Several
-- types return False at the moment, because they are not supported yet by this
-- library. Support for these types is planned for future increments.
isSuppDecl :: Decl -> Bool
isSuppDecl (TypeDecl _ _ _ _)           = False
isSuppDecl (TypeFamDecl _ _ _ _)        = False
isSuppDecl (DataDecl _ _ _ _ _ _ _)     = True
isSuppDecl (GDataDecl _ _ _ _ _ _ _ _)  = False
isSuppDecl (DataFamDecl _ _ _ _ _)      = False
isSuppDecl _                            = False

-- TODO: This is all hardcoded now. Perhaps this could be done more nicely?
adapters :: Map String LibParser
adapters = fromList [ ("emgm",     makeEMGM)
                    , ("syb",      makeSYB)
                    , ("multirec", makeMultiRec) ]

