%include polycode.fmt
\begin{code}
makeEMGM :: CodeGenerator
makeEMGM tc@(TCInfo _ TyDataType _ _) = [createDTEP tc]
\end{code}
