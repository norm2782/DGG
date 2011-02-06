%include polycode.fmt
\begin{code}
mkEPName :: Name -> String
mkEPName n = "dggEP_" ++ (fromName n)
\end{code}
