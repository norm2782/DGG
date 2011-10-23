module GHCAPI where

import GHC
import Outputable

import GHC.Paths ( libdir )
--GHC.Paths is available via cabal install ghc-paths

import DynFlags

targetFile = "TestModule.hs"

main :: IO ()
main = do
   res <- example
   putStrLn $ showSDoc ( ppr res )

example :: IO ModSummary
example =
    defaultErrorHandler defaultLogAction $ do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl xopt_set dflags
                            [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
        setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        load LoadAllTargets
        modSum <- getModSummary $ mkModuleName "TestModule"
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        l <- loadModule d
        n <- getNamesInScope
        c <- return $ coreModule d

        g <- getModuleGraph
        mapM showModule g
        return $ pm_mod_summary p -- $ (parsedSource d)--,"\n-----\n",  typecheckedSource d)

kind_ :: Bool -> String -> IO (Type, Kind)
kind_ b t =
  defaultErrorHandler defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      load LoadAllTargets
      setContext [(IIDecl (simpleImportDecl (mkModuleName "Prelude")))]
      typeKind b t

type_ :: String -> IO Type
type_ t =
  defaultErrorHandler defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      setSessionDynFlags dflags
      load LoadAllTargets
      setContext [(IIDecl (simpleImportDecl (mkModuleName "Prelude")))]
      exprType t

