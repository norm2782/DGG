module GHCAPI where

import Control.Monad
import Data.Maybe

import DynFlags
import GHC
import GHC.Paths ( libdir )
import MonadUtils
import OccName
import Outputable
import RdrName


-- DGG Imports
import qualified DGG.Types as DGG

targetFile :: String
targetFile = "examples/testmodule.hs"

main :: IO ()
main = do
   res <- example
   return ()
   {- putStrLn "parsedSource: "-}
   {- putStrLn $ showSDoc ( ppr res )-}

example :: IO ParsedSource
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
        {- d <- desugarModule t-}
        {- l <- loadModule d-}
        {- n <- getNamesInScope-}
        {- c <- return $ coreModule d-}
        {- g <- getModuleGraph-}
        {- mapM showModule g-}
        let ps = parsedSource t
        ts <- getTys ps
        mapM toDGG ts
        {- liftIO $ putStrLn "getTys:"-}
        {- liftIO $ print ts-}
        return $ ps--,"\n-----\n",  typecheckedSource d)

getTys :: Monad m => ParsedSource -> m [TyClDecl RdrName]
getTys ps = do
  let modl = unLoc ps
  let decls = hsmodDecls modl
  let hsdecls = map unLoc decls
  let tydecls = [d | TyClD d <- hsdecls]
  {- let datas = [d | d@(TyData _ _ _ _ _ _ _ _) <- tydecls]-}
  {- let tysyns = [d | d@(TySynonym _ _ _ _) <- tydecls]-}
  {- liftIO $ putStrLn $ showSDoc ( ppr datas )-}
  {- liftIO $ putStrLn $ showSDoc ( ppr tysyns )-}
  return tydecls -- datas ++ tysyns -- $ DGG.DataType {}
-- TODO: Filter datatypes based on a user-provided whitelist (or just take all of them when no whitelist is provided)


toDGG :: Monad m => TyClDecl RdrName -> m DGG.DataType
toDGG ty  | isDataDecl ty  = toDGGData ty
          | isSynDecl ty   = toDGGSyn ty
          | otherwise      = fail "Things other than datatypes and synonyms are not supported"

toDGGData :: Monad m => TyClDecl name -> m DGG.DataType
toDGGData ty = do
  when (not . null $ unLoc $ tcdCtxt ty) $
    fail "Contexts are not supported"
  when (isJust $ tcdTyPats ty) $
    fail "Type patterns are not supported"
  return $ DGG.DataType
    {  DGG.tyConName = undefined -- showRdrName (unLoc (tcdLName ty))
    ,  DGG.tyKind = DGG.KindStar -- TODO
    ,  DGG.tyVars = [] -- TODO
    ,  DGG.ctors = [] -- TODO
    }

toDGGSyn :: Monad m => TyClDecl name -> m DGG.DataType
toDGGSyn = undefined

{- kind_ :: Bool -> String -> IO (Type, Kind)-}
{- kind_ b t =-}
  {- defaultErrorHandler defaultLogAction $ do-}
    {- runGhc (Just libdir) $ do-}
      {- dflags <- getSessionDynFlags-}
      {- setSessionDynFlags dflags-}
      {- load LoadAllTargets-}
      {- setContext [(IIDecl (simpleImportDecl (mkModuleName "Prelude")))]-}
      {- typeKind b t-}

{- type_ :: String -> IO Type-}
{- type_ t =-}
  {- defaultErrorHandler defaultLogAction $ do-}
    {- runGhc (Just libdir) $ do-}
      {- dflags <- getSessionDynFlags-}
      {- setSessionDynFlags dflags-}
      {- load LoadAllTargets-}
      {- setContext [(IIDecl (simpleImportDecl (mkModuleName "Prelude")))]-}
      {- exprType t-}

