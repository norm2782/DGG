%include polycode.fmt
\begin{code}
PatBind srcLoc (PVar (Ident "listEP")) Nothing
(UnGuardedRhs (App (App (Con (UnQual (Ident "EP")))
    (Var (UnQual (Ident "from")))) (Var (UnQual (Ident "to")))))
\end{code}
