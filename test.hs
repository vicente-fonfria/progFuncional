data Expr = Lit Int
          | Expr :+ Expr
          | Expr :- Expr
    deriving(Show)


data Nivel = Bajo | Medio | Alto | Critico
  deriving (Enum, Show, Eq)
