%include polycode.fmt
\begin{code}
data TypeType = TyDataType | TyNewType | TySynonym | TyGADT

data ConFixity = Prefix | Infix | Infixl | Infixr

data TCInfo  = TCInfo  {  tcName    :: Name 
                       ,  tcType    :: TypeType
                       ,  tcVars    :: [TCVar]
                       ,  tcDCs     :: [DCInfo] }

data TCVar   = TCVar   {  tcvName   :: Name 
                       ,  tcvKind   :: Maybe Kind }

data DCInfo  = DCInfo  {  dcName    :: Name
                       ,  dcIndex   :: Int
                       ,  dcFixity  :: ConFixity
                       ,  dcVars    :: [DCVar] }

data DCVar   = DCVar   {  dcvRec    :: Maybe Name
                       ,  dcvType   :: Type }
\end{code}
