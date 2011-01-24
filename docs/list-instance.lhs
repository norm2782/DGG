%include polycode.fmt
\begin{code}
list = Cons 1 (Cons 2 (Const 3 Nil))
rep  = R (1 :*: R (2 :*: R (3 :*: Unit))) 
\end{code}
