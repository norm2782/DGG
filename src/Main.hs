{-# LANGUAGE DeriveDataTypeable #-}

module Main where

-- DGG imports
import DGG.Types

-- General imports
import Control.Monad
import Data.Char
import Data.Generics
import Data.Map (fromList, (!), Map)
import Data.Typeable
import Language.Haskell.Interpreter as H
import System.Console.CmdArgs

-- GHC related imports
import DynFlags
import GHC
import GHC.Paths ( libdir )
import HsDecls
import Outputable

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
  {- TODO: This is rather ugly. Can't CmdArgs do this kind of thing?-}
  when ((null . adapter) args || (null . inputfile) args) $
      fail "Specify at least an adapter and an input file."
  let mn | null $ modulename args  = "GenericReps"
         | otherwise               = modulename args
  {- pr <- parseFile (inputfile args)-}
  {- let code  = undefined -- genCode pr mn (makeFn adap) (isSuppFn adap) (importsFn adap)-}
  intpRes <- runInterpreter $ mkCode (adapter args) -- TODO: Pr
  case intpRes of
    (Left err)    ->  fail $ show err
    (Right code)  ->  if (not . null . outputfile) args
                        then writeFile (outputfile args) code
                        else putStrLn code

mkCode :: MonadInterpreter m => H.ModuleName -> m b
mkCode nm = do
  intpd <- H.isModuleInterpreted nm
  when (not intpd) $ loadModules ["Data.Generic.DGG.Adapters." ++ nm]
  return undefined

{- parseFile :: String -> IO ModSummary-}
{- parseFile filename =-}
  {- defaultErrorHandler defaultLogAction $ do-}
    {- runGhc (Just libdir) $ do-}
      {- dflags <- getSessionDynFlags-}
      {- let dflags' = foldl xopt_set dflags-}
                          {- [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]-}
      {- setSessionDynFlags dflags'-}
      {- target <- guessTarget filename Nothing-}
      {- setTargets [target]-}
      {- load LoadAllTargets-}
      {- mods <- getModuleGraph-}
      {- let modSum =  case mods of-} -- TODO: Keep this
                      {- []     -> error "No modules loaded"-}
                      {- (x:_)  -> x-}
      {- p <- parseModule modSum-}
      {- t <- typecheckModule p-}
      {- l <- loadModule d-}
      {- n <- getNamesInScope-}
      {- c <- return $ coreModule d-}

      {- g <- getModuleGraph-}
      {- mapM showModule g-}
      {- return $ pm_mod_summary p -- $ (parsedSource d)--,"\n-----\n",  typecheckedSource d)-}
