{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad
import Data.Char
import Data.Generics
import Data.Map (fromList, (!), Map)
import Data.Typeable
import DGG.AdapterAbstract
import DGG.Adapter.EMGM
import DGG.Adapter.MultiRec
import DGG.Adapter.SYB
import System.Console.CmdArgs

data DGGArgs = DGGArgs {
     adapter     :: String
  ,  input       :: String
--  ,  datatype    :: String
  ,  modulename  :: String
  ,  output      :: String
  }
  deriving (Show, Data, Typeable)

-- TODO: Add output module name as parameter
dgg :: DGGArgs
dgg = DGGArgs {
     adapter    = def &= help "Adapter name. E.g.: EMGM"
  ,  input      = def &= typFile &= help "Input file"
--   ,  datatype = def &= help "Specify datatype for which to derive. Generates for all datatypes if left blank."
  ,  modulename = def &= help "Name of the module. Defaults to GenericReps."
  ,  output     = def &= typFile &= help "Output file. E.g.: Instances.hs"
  }
  &= summary "DGG: Datatype Generic Generator v0.2-dev"
  &= program "dgg"

main :: IO ()
main = do
    args <- cmdArgs dgg
    -- TODO: This is rather ugly. Can't CmdArgs do this kind of thing?
    when ((null . adapter) args || (null . input) args) $
         error "Specify at least an adapter and an input file."
    let mn | null $ modulename args  = "GenericReps"
           | otherwise               = modulename args
    pr   <- parseFile (input args)
    let adap  = adapters ! map toLower (adapter args)
    let code  = genCode pr mn (makeFn adap) (isSuppFn adap) (importsFn adap)
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
                            $ concatMap (p . mkTCI) xs

-- TODO: I don't like the way this is done here...
data Adapters = EMGM | SYB | MultiRec

class Adapter a where
  makeFn     :: a -> CodeGenerator
  isSuppFn   :: a -> Decl -> Bool
  importsFn  :: a -> [ImportDecl]

instance Adapter Adapters where
  makeFn     EMGM      = makeEMGM
  makeFn     SYB       = makeSYB
  makeFn     MultiRec  = makeMultiRec
  isSuppFn   EMGM      = isSuppEMGM
  isSuppFn   SYB       = isSuppSYB
  isSuppFn   MultiRec  = isSuppMultiRec
  importsFn  EMGM      = importsEMGM
  importsFn  SYB       = importsSYB
  importsFn  MultiRec  = importsMultiRec

-- TODO: Re-implement somehow
adapters :: Map String Adapters
adapters = fromList  [  ("emgm",      EMGM)
                     ,  ("syb",       SYB)
                     ,  ("multirec",  MultiRec) ]
