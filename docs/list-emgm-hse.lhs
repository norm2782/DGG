%include polycode.fmt
\begin{code}
PatBind srcLoc (PVar (Ident "listEP")) Nothing
(UnGuardedRhs (App (App (Con (UnQual (Ident "EP")))
    (Var (UnQual (Ident "from")))) (Var (UnQual (Ident "to")))))
(BDecls
[  FunBind
     [  Match srcLoc (Ident "from") [PApp (UnQual (Ident "Nil")) []]
          Nothing (UnGuardedRhs (App (Con (UnQual (Ident "L")))
          (Con (UnQual (Ident "Unit"))))) (BDecls [])
     ,  Match srcLoc (Ident "from") [PParen (PApp (UnQual
          (Ident "Cons")) [PVar (Ident "x"),PVar (Ident "xs")])]
          Nothing (UnGuardedRhs (App (Con (UnQual (Ident "R")))
          (Paren (InfixApp (Var (UnQual (Ident "x"))) (QConOp (UnQual
          (Symbol ":*:"))) (Var (UnQual (Ident "xs"))))))) (BDecls [])]
,  FunBind
     [  Match srcLoc (Ident "to") [PParen (PApp (UnQual
          (Ident "L")) [PApp (UnQual (Ident "Unit")) []])] Nothing
          (UnGuardedRhs (Con (UnQual (Ident "Nil")))) (BDecls [])
     ,  Match srcLoc (Ident "to") [PParen (PApp (UnQual
          (Ident "R")) [PParen (PInfixApp (PVar (Ident "x"))
          (UnQual (Symbol ":*:")) (PVar (Ident "xs")))])] Nothing
          (UnGuardedRhs (App (App (Con (UnQual (Ident "Cons")))
          (Var (UnQual (Ident "x")))) (Var (UnQual (Ident "xs")))))
          (BDecls [])]])
\end{code}
