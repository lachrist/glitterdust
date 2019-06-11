
module Control(Variable(V), Control(Lambda, Call, Namespace, Def, Set, Get, Branch, Constant, StoredConstant), parse) where

import qualified Parser
import qualified State
import qualified Store
import qualified Control.Monad
import qualified Control.Monad.State
import qualified Control.Monad.Trans.Class
import qualified Control.Applicative
import qualified Data.Foldable
import qualified Primitive

newtype Variable = V String deriving (Eq, Show)

type CStore = Store.Store Control
type CAddress = Store.Address Control
type CStoreParser = State.StateT CStore Parser.Parser

data Control = Lambda [Variable] CAddress
             | Call CAddress [CAddress]
             | Namespace
             | Def Variable CAddress
             | Set Variable CAddress
             | Get Variable
             | Branch CAddress CAddress CAddress
             | Constant Primitive.Primitive
             | StoredConstant Primitive.Primitive
             deriving (Eq, Show)

parse :: String -> CStore -> Either String (CAddress, CStore)
parse s cs = case Parser.apply (State.apply control cs) s
             of [] -> Left "no parsing"
                [(x, "")] -> Right x
                [(_, s)] -> Left $ "parsing reminder: " ++ s
                _ -> Left "multiple parsing (should never happend)"

-------------
-- Helpers --
-------------

spaces :: Parser.Parser ()
spaces = (Control.Applicative.many $ comment Control.Applicative.<|> blank) *> return ()
  where blank = Parser.sat (`elem` [' ', '\t', '\n']) *> return ()
        comment = Parser.char ';' *> Control.Applicative.many (Parser.sat (/='\n')) *> Parser.char '\n'

variable :: CStoreParser Variable
variable = Control.Monad.Trans.Class.lift $ spaces *> Control.Monad.liftM V (Control.Applicative.some $ Parser.sat (`notElem` [' ', '\t', '\n', '(', ')', ';']))

parenthesis :: CStoreParser a -> CStoreParser a
parenthesis p = (Control.Monad.Trans.Class.lift $ spaces *> Parser.char '(') *> p <* (Control.Monad.Trans.Class.lift $ spaces *> Parser.char ')')

sexpression :: String -> CStoreParser a -> CStoreParser a
sexpression s p = parenthesis $ (Control.Monad.Trans.Class.lift $ spaces *> Parser.string s) *> p

--------------------------
-- CStoreParser Control --
--------------------------

lambda :: CStoreParser Control
lambda = sexpression "lambda" (Control.Applicative.liftA2 Lambda (parenthesis $ Control.Applicative.many variable) control)

branch :: CStoreParser Control
branch = sexpression "if" (Control.Applicative.liftA3 Branch control control control)

namespace :: CStoreParser Control
namespace = sexpression "current-namespace" (pure Namespace)

def :: CStoreParser Control
def = sexpression "define" (Control.Applicative.liftA2 Def variable control)

set :: CStoreParser Control
set = sexpression "set!" (Control.Applicative.liftA2 Set variable control)

call :: CStoreParser Control
call = parenthesis $ Control.Applicative.liftA2 Call control (Control.Applicative.many control)

constant :: CStoreParser Control
constant = Control.Applicative.liftA Constant (Control.Monad.Trans.Class.lift $ spaces *> Parser.fromRead)

get :: CStoreParser Control
get = Control.Applicative.liftA Get variable

---------------
-- Top Level --
---------------

control :: CStoreParser CAddress
control = do c <- Data.Foldable.asum [lambda, branch, namespace, def, set, call, constant, get]
             cs <- State.get
             State.put $ Store.add c cs
             return $ Store.nxt cs
