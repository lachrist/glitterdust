
module Expression(Variable(V), Expression(Lambda, Begin, Call, Eval, Define, Set, If, Lookup, Constant, StoredConstant), parse, visit) where

import qualified Parser
import qualified Store
import qualified Control.Monad
import qualified Control.Monad.State
import qualified Control.Applicative
import qualified Data.Foldable
import qualified Primitive

newtype Variable = V String deriving (Eq, Show)

type CAddress = Store.Address Control

data Control = Lambda [Variable] Expression
             | Call Expression [Expression]
             | Eval Expression
             | Def Variable Expression
             | Set Variable Expression
             | Branch Expression Expression Expression
             | Get Variable 
             | Constant Primitive.Primitive
             | StoredConstant Primitive.Primitive
             deriving (Eq, Show)

parse :: Parser.Parser Expression
parse = expr <* spaces
  where comment = Parser.char ';' >> Control.Applicative.many (Parser.sat (/='\n')) >> Parser.char '\n'
        blank = Parser.sat (`elem` [' ', '\t', '\n']) >> return ()
        spaces = Control.Applicative.many $ comment Control.Applicative.<|> blank
        parenth p = (spaces >> Parser.char '(') >> p <* (spaces >> Parser.char ')')
        sexpr s p = parenth $ spaces >> Parser.string s >> p
        var       = spaces >> Control.Monad.liftM V (Control.Applicative.some $ Parser.sat (`notElem` [' ', '\t', '\n', '(', ')', ';']))
        expr      = Data.Foldable.asum [lambda, eval, set, def, branch, call, constant, get]
        lambda    = sexpr "lambda" $ Control.Monad.liftM2 Lambda (parenth $ Control.Applicative.many var) expr
        eval      = sexpr "eval"   $ Control.Monad.liftM Eval expr
        def       = sexpr "define" $ Control.Monad.liftM2 Def var expr
        set       = sexpr "set!"   $ Control.Monad.liftM2 Set var expr
        branch    = sexpr "if"     $ Control.Monad.liftM3 If expr expr expr
        call      = parenth $ Control.Monad.liftM2 Call expr (Control.Applicative.many expr)
        constant  = Control.Monad.liftM Constant (spaces >> Parser.fromRead)
        get       = Control.Monad.liftM Get var

type Visitor = (Int -> Expression -> Expression)

visit :: Visitor -> Expression -> Control.Monad.State.State Int Expression
visit v (Lambda strs expr)         = visit1 v (Lambda strs) expr
visit v (Def str expr)             = visit1 v (Def str) expr 
visit v (Set str expr)             = visit1 v (Set str) expr 
visit v (Eval expr)                = visit1 v Eval expr 
visit v (Call expr exprs)          = do expr' <- visit v expr
                                        exprs' <- sequence $ map (visit v) exprs
                                        increment v (Call expr' exprs')
visit v (Branch expr1 expr2 expr3) = do expr1' <- visit v expr1
                                        expr2' <- visit v expr2
                                        expr3' <- visit v expr3
                                        increment v (If expr1' expr2' expr3')

visit1 :: Visitor -> (Expression -> Expression) -> Expression -> Control.Monad.State.State Int Expression
visit1 v f expr = visit v expr >>= (increment v) . f

increment :: Visitor -> Expression -> Control.Monad.State.State Int Expression
increment v expr = Control.Monad.State.modify (+1) >> Control.Monad.State.get >>= (return . flip v expr)
