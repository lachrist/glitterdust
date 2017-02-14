
module Kontinuation(Kontinuation(If, Set, Define, Eval, Begin, Call, Halt)) where

import qualified Expression
import qualified Environment
import qualified Store

type EAddress v = Store.Address (Environment.Environment v)
type KAddress v = Store.Address (Kontinuation v)

data Kontinuation v = Branch Expression.Expression Expression.Expression (EAddress v) (KAddress v)
                    | Set Expression.Variable (EAddress v) (KAddress v)
                    | Def Expression.Variable (EAddress v) (KAddress v)
                    | Eval (EAddress v) (KAddress v)
                    | Begin [Expression.Expression] (EAddress v) (KAddress v)
                    | Call [Expression.Expression] [v] (EAddress v) (KAddress v)
                    | Halt
                    deriving (Eq, Show)
