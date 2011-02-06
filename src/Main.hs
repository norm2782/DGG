{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Char (toLower)
import Data.Generics
import Data.Map (fromList, (!), Map)
import Data.Typeable
import DGG.AdapterAbstract
import DGG.Adapter.EMGM
import DGG.Adapter.MultiRec
import DGG.Adapter.SYB
import System.Console.CmdArgs

data Adapter = Adapter { makeFn    :: CodeGenerator
                       , isSuppFn  :: (Decl -> Bool)
                       , importsFn :: [ImportDecl] }

data DGGArgs = DGGArgs { adapter  :: String
                       , input    :: String
--                       , datatype :: String
                       , modulename  :: String
                       , output   :: String
                       }
                       deriving (Show, Data, Typeable)

-- TODO: Add output module name as parameter
dgg :: DGGArgs
dgg = DGGArgs { adapter    = def &= help "Adapter name. E.g.: EMGM"
              , input      = def &= typFile &= help "Input file"
--              , datatype = def &= help "Specify datatype for which to derive. Generates for all datatypes if left blank."
              , modulename = def &= help "Name of the module. Defaults to GenericReps."
              , output     = def &= typFile &= help "Output file. E.g.: Instances.hs"
              }
              &= summary "DGG: Datatype Generic Generator v0.1.1-dev"
              &= program "dgg"

main :: IO ()
main = do
    args <- cmdArgs dgg
    -- TODO: This is rather ugly. Can't CmdArgs do this kind of thing?
    if (null $ adapter args) || (null $ input args)
        then error "Specify at least an adapter and an input file."
        else return ()
    mn <- if (null $ modulename args)
            then return "GenericReps"
            else return $ modulename args
    pr   <- parseFile (input args)
    adap <- return $ adapters ! (map toLower $ adapter args)
    code <- return $ genCode pr mn (makeFn adap) (isSuppFn adap) (importsFn adap)
    if (not . null . output) args
        then writeFile (output args) code
        else putStrLn code

genCode :: ParseResult Module -> String -> CodeGenerator -> LibSupport
        -> [ImportDecl] -> String
genCode (ParseFailed l m) _ _ _ _  = error $ "Failed to parse module."
                                          ++ "Error on line " ++ show l ++ ": "
                                          ++ m
genCode (ParseOk m)       n p s is = prettyPrint (mkModule n p is $ listify s m)

mkModule :: String -> CodeGenerator -> [ImportDecl] -> [Decl] -> Module
mkModule _ _ _  [] = error "No compatible datatypes found."
mkModule n p is xs = Module srcLoc (ModuleName n) [] Nothing Nothing is
                       $ concat $ map (p . mkTCI) xs

adapters :: Map String Adapter
adapters = fromList [ ("emgm",     Adapter makeEMGM     isSuppEMGM     importsEMGM)
                    , ("syb",      Adapter makeSYB      isSuppSYB      importsSYB)
                    , ("multirec", Adapter makeMultiRec isSuppMultiRec importsMultiRec) ]
