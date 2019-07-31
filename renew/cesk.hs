
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}

import qualified Data.Map.Strict
import qualified Text.Parsec
import qualified Text.Parsec.String
import qualified Data.Word
import qualified Data.Int
import qualified Data.Char
import qualified System.Environment
import qualified Data.List

----------
-- Data --
----------

type Number = Data.Int.Int32

type Builtin = String

data Data x = Number Number
            | Builtin Builtin
            | Cons x x
            | Closure (Environment x) [Identifier] Control
            deriving (Eq, Show)

-------------
-- Control --
-------------

type Identifier = String

data Control = Literal Number
             | Get Identifier
             | If Control Control Control
             | Let Identifier Control Control
             | Lambda [Identifier] Control
             | Apply Control [Control]
             deriving (Eq, Show)

-----------------
-- Environment --
-----------------

type Environment x = Data.Map.Strict.Map Identifier x

-----------
-- Store --
-----------

type Address = Data.Word.Word32

type Store y = Data.Map.Strict.Map Address y

------------------
-- Kontinuation --
------------------

data Kontinuation x = LetKont (Environment x) Identifier Control (Kontinuation x)
                    | ApplyKont (Environment x) [Control] [x] (Kontinuation x)
                    | IfKont (Environment x) Control Control (Kontinuation x)
                    | SuccessKont
                    deriving (Show, Eq)

-----------
-- State --
-----------

data State x y = Ongoing Control (Environment x) (Store y) (Kontinuation x)
               | Success (Store y) x
               | Failure String
               deriving (Show, Eq)

------------
-- System --
------------

class (Eq x, Eq y, Show x, Show y) => System x y where
  dataToValue :: Store y -> Data x -> (Store y, x)
  valueToData :: Store y -> x -> Data x
  mutate :: Store y -> x -> Data x -> Maybe (Store y)

-----------------
-- Interpreter --
-----------------

step :: (System x y) => State x y -> State x y
step (Ongoing (Literal ν) _ σ κ) = kontinue κ (dataToValue σ (Number ν))
step (Ongoing (Lambda ηs γ) ε σ κ) = kontinue κ (dataToValue σ (Closure ε ηs γ))
step (Ongoing (Get η) ε σ κ) = maybe (Failure $ "Undeclared identifier: " ++ η)
                                     (kontinue κ . (σ,))
                                     (Data.Map.Strict.lookup η ε)
step (Ongoing (Let η γ γ') ε σ κ) = Ongoing γ ε σ (LetKont ε η γ' κ)
step (Ongoing (Apply γ γs) ε σ κ) = Ongoing γ ε σ (ApplyKont ε γs [] κ)
step (Ongoing (If γ γ' γ'') ε σ κ) = Ongoing γ ε σ (IfKont ε γ' γ'' κ)
step φ = error "Not stepping ongoing state"

kontinue :: (System x y) => Kontinuation x -> (Store y, x) -> State x y
kontinue SuccessKont (σ, ξ) = Success σ ξ
kontinue (IfKont ε γ γ' κ) (σ, ξ) = case valueToData σ ξ
                                    of (Number 0) -> Ongoing γ' ε σ κ
                                       _          -> Ongoing γ  ε σ κ
kontinue (LetKont ε η γ κ) (σ, ξ) = Ongoing γ (Data.Map.Strict.insert η ξ ε) σ κ
kontinue (ApplyKont ε (γ:γs) ξs κ) (σ, ξ) = Ongoing γ ε σ (ApplyKont ε γs (ξs ++ [ξ]) κ)
kontinue (ApplyKont ε [] ξs κ) (σ, ξ) = case valueToData σ (head $ ξs ++ [ξ])  of
  (Closure ε ηs γ) -> Ongoing γ (Data.Map.Strict.union (Data.Map.Strict.fromList (zip ηs (tail ξs ++ [ξ]))) ε) σ κ
  (Builtin β) -> maybe (Failure $ "Builtin (" ++ β ++ ") application failure")
                       (kontinue κ)
                       (applyBuiltin β (tail $ ξs ++ [ξ]) σ)

applyBuiltin :: (System x y) => String -> [x] -> Store y -> Maybe (Store y, x)
applyBuiltin "eq?" (ξ:ξ':[]) σ = Just $ dataToValue σ (Number (if ξ == ξ' then 1 else 0))
applyBuiltin "cons" (ξ:ξ':[]) σ = Just $ dataToValue σ (Cons ξ ξ')
applyBuiltin "car" (ξ:[]) σ = consOnly (valueToData σ ξ) (Just . (σ,) . fst)
applyBuiltin "cdr" (ξ:[]) σ = consOnly (valueToData σ ξ) (Just . (σ,) . snd)
applyBuiltin "set-car!" (ξ:ξ':[]) σ = consOnly (valueToData σ ξ) (\(_,ξ'') -> fmap (,ξ) (mutate σ ξ (Cons ξ' ξ'')))
applyBuiltin "set-cdr!" (ξ:ξ':[]) σ = consOnly (valueToData σ ξ) (\(ξ'',_) -> fmap (,ξ) (mutate σ ξ (Cons ξ'' ξ')))
applyBuiltin β ξs σ = fmap (dataToValue σ) (applyPure β (map (valueToData σ) ξs))

applyPure :: String -> [Data v] -> Maybe (Data v)
applyPure "="  (Number ν : Number ν' : []) = Just $ Number $ if ν == ν' then 1 else 0
applyPure "<"  (Number ν : Number ν' : []) = Just $ Number $ if ν <  ν' then 1 else 0
applyPure "<=" (Number ν : Number ν' : []) = Just $ Number $ if ν <= ν' then 1 else 0
applyPure "+"  (Number ν : Number ν' : []) = Just $ Number $ ν +      ν'
applyPure "-"  (Number ν : Number ν' : []) = Just $ Number $ ν -      ν'
applyPure "*"  (Number ν : Number ν' : []) = Just $ Number $ ν *      ν'
applyPure "/"  (Number ν : Number ν' : []) = Just $ Number $ ν `quot` ν'
applyPure "%"  (Number ν : Number ν' : []) = Just $ Number $ ν `mod`  ν'
applyPure _ _  = Nothing

-------------
-- Helpers --
-------------

builtins :: [Builtin]
builtins = ["eq?", "cons", "car", "cdr", "set-car!", "set-cdr!", "=", "<=", "+", "-", "*", "/", "%"]

accumulator :: (System x y) => (Store y, Environment x) -> Builtin -> (Store y, Environment x)
accumulator (σ, ε) β = let (σ', ξ) = dataToValue σ (Builtin β)
                       in (σ', Data.Map.Strict.insert β ξ ε)

execute :: (System x y) => Control -> State x y
execute γ = let (σ, ε) = Data.List.foldl' accumulator (Data.Map.Strict.empty, Data.Map.Strict.empty) builtins
            in run $ Ongoing γ ε σ SuccessKont
  where accumulator (σ, ε) β = let (σ', ξ) = dataToValue σ (Builtin β)
                               in (σ', Data.Map.Strict.insert β ξ ε)

-- execute :: (System x y) => Control -> State x y
-- execute γ = let (σ, ε) = accumulator (Data.Map.Strict.empty, Data.Map.Strict.empty) "eq"
--             in run $ Ongoing γ ε σ SuccessKont

executeSystem :: String -> Control -> String
executeSystem "inline" γ = show (execute γ :: State ValueInline ElementInline)
executeSystem "mixed" γ = show (execute γ :: State ValueMixed ElementMixed)
executeSystem "stored" γ = show (execute γ :: State ValueStored ElementStored)
executeSystem "stored-reuse" γ = show (execute γ :: State ValueStoredReuse ElementStoredReuse)
executeSystem λ γ = "Unknown system: " ++ λ

top :: String -> String -> IO String
top λ ρ = Text.Parsec.String.parseFromFile parserControl ρ >>= return . (either show (executeSystem λ))

run :: (System x y) => State x y -> State x y
run φ@(Ongoing _ _ _ _) = run $ step φ
run φ = φ

fresh :: Store y -> Address
fresh σ = maybe 0 ((+1) . fst) (Data.Map.Strict.lookupMax σ)

reuse :: (Eq y) => Store y -> y -> Address
reuse σ ψ = maybe (fresh σ)
                  id
                  (Data.Map.Strict.foldrWithKey (\α ψ' μ -> if ψ == ψ' then Just α else μ) Nothing σ)

consOnly :: Data x -> ((x, x) -> Maybe z) -> Maybe z
consOnly (Cons ξ ξ') f = f (ξ, ξ')
consOnly _ _ = Nothing

parserControl = Text.Parsec.choice [Text.Parsec.try parserLiteral, Text.Parsec.try parserGet, Text.Parsec.try parserIf, Text.Parsec.try parserLet, Text.Parsec.try parserLambda, parserApply]

parserIdentifier = Text.Parsec.spaces >> Text.Parsec.many1 (Text.Parsec.satisfy (\c -> (not (Data.Char.isSpace c)) && (c /= '(') && (c /= ')')))

parserLiteral = do Text.Parsec.spaces
                   μ <- Text.Parsec.optionMaybe (Text.Parsec.char '-')
                   ν <- Text.Parsec.many1 Text.Parsec.digit
                   return $ Literal $ maybe (read ν) (const (negate $ read ν)) μ

parserGet = fmap Get parserIdentifier

parserIf = do Text.Parsec.spaces >> Text.Parsec.char '('
              Text.Parsec.spaces >> Text.Parsec.string "if"
              γ <- parserControl
              γ' <- parserControl
              γ'' <- parserControl
              Text.Parsec.spaces >> Text.Parsec.char ')'
              return $ If γ γ' γ''

parserLet = do Text.Parsec.spaces >> Text.Parsec.char '('
               Text.Parsec.spaces >> Text.Parsec.string "let"
               η <- parserIdentifier
               γ <- parserControl
               γ' <- parserControl
               Text.Parsec.spaces >> Text.Parsec.char ')'
               return $ Let η γ γ'

parserLambda = do Text.Parsec.spaces >> Text.Parsec.char '('
                  Text.Parsec.spaces >> Text.Parsec.string "lambda"
                  Text.Parsec.spaces >> Text.Parsec.char '('
                  ηs <- Text.Parsec.many parserIdentifier
                  Text.Parsec.spaces >> Text.Parsec.char ')'
                  γ <- parserControl
                  Text.Parsec.spaces >> Text.Parsec.char ')'
                  return $ Lambda ηs γ

parserApply = do Text.Parsec.spaces >> Text.Parsec.char '('
                 γ <- parserControl
                 γs <- Text.Parsec.many parserControl
                 Text.Parsec.spaces >> Text.Parsec.char ')'
                 return $ Apply γ γs

-------------------
-- System Stored --
-------------------

data ValueStored = AddressStored Address deriving (Show, Eq)
data ElementStored = ElementStored (Data ValueStored) deriving (Show, Eq)

instance System ValueStored ElementStored where
  dataToValue σ δ = (Data.Map.Strict.insert (fresh σ) (ElementStored δ) σ, AddressStored (fresh σ))
  valueToData σ (AddressStored α) = let (ElementStored δ) = σ Data.Map.Strict.! α in δ
  mutate σ (AddressStored α) δ = Just $ Data.Map.Strict.insert α (ElementStored δ) σ

-------------------------
-- System StoredReuse  --
-------------------------

data ValueStoredReuse = AddressStoredReuse Address deriving (Show, Eq)
data ElementStoredReuse = ElementStoredReuse (Data ValueStoredReuse) deriving (Show, Eq)

instance System ValueStoredReuse ElementStoredReuse where
  dataToValue σ δ = (Data.Map.Strict.insert (reuse σ (ElementStoredReuse δ)) (ElementStoredReuse δ) σ, AddressStoredReuse (fresh σ))
  valueToData σ (AddressStoredReuse α) = let (ElementStoredReuse δ) = σ Data.Map.Strict.! α in δ
  mutate σ (AddressStoredReuse α) δ = Just $ Data.Map.Strict.insert α (ElementStoredReuse δ) σ

-------------------
-- System Inline --
-------------------

data ValueInline = ValueInline (Data ValueInline) deriving (Show, Eq)
data ElementInline deriving (Show, Eq)

instance System ValueInline ElementInline where
  dataToValue σ δ = (σ, ValueInline δ)
  valueToData σ (ValueInline δ) = δ
  mutate _ _ _ = Nothing

------------------
-- System Mixed --
------------------

data ValueMixed = NumberMixed Number
                | BuiltinMixed Builtin
                | ClosureMixed (Environment ValueMixed) [Identifier] Control
                | AddressMixed Address
                deriving (Show, Eq)

data ElementMixed = ConsMixed ValueMixed ValueMixed deriving (Show, Eq)

instance System ValueMixed ElementMixed where
  dataToValue σ (Number ν) = (σ, NumberMixed ν)
  dataToValue σ (Builtin β) = (σ, BuiltinMixed β)
  dataToValue σ (Closure ε ηs γ) = (σ, ClosureMixed ε ηs γ)
  dataToValue σ (Cons ξ ξ') = (Data.Map.Strict.insert (fresh σ) (ConsMixed ξ ξ') σ, AddressMixed $ fresh σ)
  valueToData σ (NumberMixed ν) = Number ν
  valueToData σ (BuiltinMixed β) = Builtin β
  valueToData σ (ClosureMixed ε ηs γ) = Closure ε ηs γ
  valueToData σ (AddressMixed α) = case σ Data.Map.Strict.! α of (ConsMixed ξ ξ') -> Cons ξ ξ'
  mutate σ (AddressMixed α) (Cons ξ ξ') = Just $ Data.Map.Strict.insert α (ConsMixed ξ ξ') σ
  mutate _ _ _ = Nothing

----------
-- Main --
----------

main :: IO ()
main = do ξs <- System.Environment.getArgs
          if length ξs /= 2
          then putStrLn "Usage: cesk (inline|mixed|stored|stored-reuse) program.lsp"
          else top (head ξs) (head $ tail ξs) >>= putStrLn

----------
-- Test --
----------
-- 
-- fac6 :: Control
-- fac6 = (Let "fac-cons"
--   (Application
--     (Identifier "cons")
--     [
--       (Literal $ Null),
--       (Literal $ Null)])
--   (
--     Let "fac"
--     (Abstraction ["x"]
--       (If
--         (Application
--           (Identifier "<")
--           [
--             (Identifier "x"),
--             (Literal $ Float 1)])
--         (Literal $ Float 1)
--         (Application
--           (Identifier "*")
--           [
--             (Identifier "x"),
--             (Application
--               (Application
--                 (Identifier "car")
--                 [
--                   (Identifier "fac-cons")])
--               [
--                 (Application
--                   (Identifier "-")
--                   [
--                     (Identifier "x"),
--                     (Literal $ Float 1)])])])))
--     (Let "_"
--       (Application
--         (Identifier "set-car!")
--         [
--           (Identifier "fac-cons"),
--           (Identifier "fac")])
--       (Application
--         (Identifier "fac")
--         [
--           (Literal $ Float 6)]))))
-- 
-- yo1 :: Final Value1 Element1
-- yo1 = eval1 fac6
-- 
-- yo2 :: Final Value2 Element2
-- yo2 = eval2 fac6
-- 
-- yo3 :: Final Value3 Element3
-- yo3 = eval3 fac6
