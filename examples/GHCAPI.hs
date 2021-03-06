module GHCAPI where

import Control.Monad
import qualified Data.Map as Map
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
  defaultErrorHandler defaultLogAction $
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
      ts <- getTyDecls ps
      mapM_ toDGG ts
      {- liftIO $ putStrLn "getTys:"-}
      {- liftIO $ print ts-}
      return ps--,"\n-----\n",  typecheckedSource d)

getTyDecls :: Monad m => ParsedSource -> m [TyClDecl RdrName]
getTyDecls ps = return [d | TyClD d <- map unLoc (hsmodDecls $ unLoc ps)]
-- TODO: Filter datatypes based on a user-provided whitelist (or just take all
-- of them when no whitelist is provided)


newtype GHCDataDecl = GHCDataDecl (TyClDecl RdrName)
newtype GHCTySynDecl = GHCTySynDecl (TyClDecl RdrName)

toDGG :: Monad m => TyClDecl RdrName -> m DGG.DataType
toDGG ty  | isDataDecl ty  = toDGGData $ GHCDataDecl ty
          | isSynDecl ty   = toDGGSyn $ GHCTySynDecl ty
          | otherwise      = fail "Things other than datatypes and synonyms are not supported"

toDGGData :: Monad m => GHCDataDecl -> m DGG.DataType
toDGGData (GHCDataDecl ty) = do
  unless (null (unLoc $ tcdCtxt ty)) $
    fail "Contexts are not supported"
  when (isJust $ tcdTyPats ty) $
    fail "Type patterns are not supported"
  tvs  <- mapM (mkTyVar . unLoc)  (tcdTyVars ty)
  cts  <- mapM (mkCtor . unLoc)   (tcdCons ty)
  -- TODO: pass around IdentMap:
  nm <- either (fail . (++) "No identifier map for ") return $
    toDGGName Map.empty (unLoc (tcdLName ty))
  return DGG.DataType
    {  DGG.tyConName  = nm
    ,  DGG.tyKind     = DGG.KindStar -- TODO
    ,  DGG.tyVars     = tvs
    ,  DGG.ctors      = cts
    }

mkTyVar :: (Outputable a, Monad m) => HsTyVarBndr a -> m String
mkTyVar (UserTyVar n ptck)  = return $ showSDoc ( ppr n )
mkTyVar _                   = fail "Explicitly kinded type variables are not supported yet"

mkCtor :: Monad m => ConDecl name -> m DGG.Con
mkCtor cdcl = return DGG.Con
  {  DGG.conName    = undefined
  ,  DGG.conFixity  = undefined
  ,  DGG.conFields  = undefined
  }

toDGGSyn :: Monad m => GHCTySynDecl -> m DGG.DataType
toDGGSyn (GHCTySynDecl syn) = undefined

type IdentMap = Map.Map String String

-- Determine if the rname is a symbolic operator. Look up the sname in the
-- identifier map. If it is symbolic and not in the map, fail with Left sname.
-- Otherwise, the result is the sname and (optionally for Ident) the mapped
-- identifier name.
toDGGName :: IdentMap -> RdrName -> Either String DGG.Name
toDGGName idmap rname = mk $ Map.lookup sname idmap
  where
    mk  | isSymOcc oname  = maybe (Left sname) (Right . DGG.Symbol sname)
        | otherwise       = Right . DGG.Ident sname
    oname = rdrNameOcc rname
    sname = occNameString oname

test_toDGGName = and $ map (\(m, n, r) -> toDGGName m n == r) tests
  where
    tests =
      [  (Map.empty, mkRdrUnqual (mkTcOcc "X"), Right $ DGG.Ident "X" Nothing)
      ,  (Map.empty, mkRdrUnqual (mkVarOcc "X"), Right $ DGG.Ident "X" Nothing)
      ,  (Map.empty, mkRdrUnqual (mkDataOcc "X"), Right $ DGG.Ident "X" Nothing)
      ,  (Map.empty, mkRdrQual (mkModuleName "M") (mkTcOcc "X"), Right $ DGG.Ident "X" Nothing)
      ,  (Map.singleton "X" "Y", mkRdrUnqual (mkTcOcc "X"), Right $ DGG.Ident "X" (Just "Y"))
      ,  (Map.empty, mkRdrUnqual (mkTcOcc ":*"), Left ":*")
      ,  (Map.singleton ":*" "C", mkRdrUnqual (mkTcOcc ":*"), Right $ DGG.Symbol ":*" "C")
      ]

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

