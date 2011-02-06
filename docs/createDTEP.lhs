%include polycode.fmt
\begin{code}
createDTEP :: TCInfo -> Decl
createDTEP (TCInfo tn TyDataType _ dcis) =
  PatBind srcLoc (mkPIdent $ mkEPName tn) Nothing
  (UnGuardedRhs (App (App (Con $ mkUId "EP")
  (mkIdent "from")) (mkIdent "to")))
  (BDecls  [  FunBind $ map (bdeclFrom  ln) dcis
           ,  FunBind $ map (bdeclTo    ln) dcis ])
  where ln = length dcis
\end{code}
