
module Structure(Structure(Stored, Closure, Builtin, Cons, VCons)) where

import qualified Expression
import qualified Environment
import qualified Store
import qualified Primitive

type EAddress v = Store.Address (Environment.Environment v)

data Structure v = Stored Primitive.Primitive
                | Closure [Expression.Variable] Expression.Expression (EAddress v)
                | Builtin String
                | Cons v v
                | VCons v v v v
                deriving (Eq, Show)
