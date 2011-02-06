%include polycode.fmt
\begin{code}
isSuppEMGM :: Decl -> Bool
isSuppEMGM (DataDecl _ _ _ _ _ _ _)    = True
isSuppEMGM _                           = False
\end{code}
