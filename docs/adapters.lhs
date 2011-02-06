%include polycode.fmt
\begin{code}
adapters :: Map String Adapter
adapters = fromList [ ("emgm", Adapter  makeEMGM isSuppEMGM
                                        importsEMGM) ]
\end{code}
