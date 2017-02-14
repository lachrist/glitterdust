
import qualified Expression
import qualified Control.Monad.State

--------------
-- Builders --
--------------

constant :: (Primitive.Wrappable w) => w -> Expression
constant w = Expression.Constant $ Primitive.wrap w

lookup :: String -> Expression
lookup s = Expression.Lookup $ Expression.V s

list :: [Expression] -> Expression
list [] = constant ()
list (e:es) = Expression.Call (Expression.Lookup $ Expression.V "cons") [e, list es]

----------------
-- Instrument --
----------------

type Locator :: (String, Int)

instrument :: String -> Expression.Expression -> Expression.Expression
instrument s e = Control.Monad.State.runState (Expression.visit f) 0
  where f i e@(Expression.Lambda _ _)       = trap "$lambda" [e] (s, i)
        f i e@(Expression.Constant _ _)     = trap "$constant" [e] (s, i)
        f i   (Expression.Call e es)        = trap "$call" [e, list es] (s, i)
        f i   (Expression.Branch e1 e2 e3)  = Branch (trap "$branch" [e1] (s, i)) e2 e3
        f i   (Expression.Eval e)           = Eval $ trap "$eval" [e] (s, i)
        f i e@(Expression.Get v)            = trap "$get" [constant $ show v, e] (s, i)
        f i   (Expression.Set v e)          = environment (Expression.Set v)    (trap "$set"    [constant $ show v, e] (s, i))
        f i   (Expression.Def v e)          = environment (Expression.Define v) (trap "$def" [constant $ show v, e] (s, i))

environment :: (Expression.Expression -> Expression.Expression) -> Expression -> Expression
environment f e = Begin [Define "$" e, f $ Call (lookup "car") [lookup "$"], Call (lookup "cdr") [lookup "$"]]

trap :: String -> [Expression] -> Locator -> Expression
trap s1 es (s2,i) = Expression.Call (lookup s1) (es ++ [constant $ s2 ++ ('@':show i)])
