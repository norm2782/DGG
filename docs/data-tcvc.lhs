%include polycode.fmt
\begin{code}
data TCInfo = TCInfo  {  tcName  ::  Name 
                      ,  tcType  ::  TypeType
                      ,  tcVars  ::  [TCVar]
                      ,  tcDCs   ::  [DCInfo]
                      }

data DCInfo = DCInfo  {  dcName     ::  Name
                      ,  dcIndex    ::  Int
                      ,  dcFixity   ::  ConFixity
                      ,  dcAssoc    ::  Associativity
                      ,  dcVars     ::  [DCVar]
                      }
\end{code}
