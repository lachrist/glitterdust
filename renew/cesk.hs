
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}

import qualified Data.Map.Strict
import qualified Text.Parsec
import qualified Text.Parsec.String
import qualified Data.Word
import qualified Data.Char
import qualified System.Environment
import qualified Data.List
import qualified Control.Monad
import qualified Text.Parsec.Token

----------------
-- ShowIndent --
----------------

class ShowIndent a where
  showIndent :: Int -> a -> String

indent :: Int -> String
indent ι = "\n" ++ (take (2 * ι) (repeat ' '))

indentOne :: ShowIndent a => Int -> a -> String
indentOne ι ν = indent (ι + 1) ++ showIndent (ι + 1) ν

indentAll :: ShowIndent a => Int -> [a] -> String
indentAll ι νs = concat $ map (indentOne ι) νs

escape :: String -> String
escape ν = "\"" ++ concat (map repl ν) ++ "\""
  where repl '\n' = "\\n"
        repl '\t' = "\\t"
        repl '"' = "\\\""
        repl ν = [ν]

----------
-- Data --
----------

type Number = Float

type Builtin = String

data Data x = Null
            | Bool Bool
            | Number Number
            | String String
            | Builtin Builtin
            | Cons x x
            | Closure (Environment x) [Identifier] Control
            deriving (Eq, Show)

showData :: Data x -> String
showData (Null) = "#n"
showData (Bool True) = "#t"
showData (Bool False) = "#f"
showData (Number ν) = show ν
showData (String ν) = escape ν
showData (Builtin ν) = "#<" ++ ν ++ ">"
showData (Cons _ _) = "#<Cons>"
showData (Closure _ _ _) = "<#Closure>"

instance (ShowIndent x) => ShowIndent (Data x) where
  showIndent ι (Cons ξ ξ') = "(cons" ++ indentOne ι ξ ++ indentOne ι ξ' ++ ")"
  showIndent ι (Closure ε ηs γ) = "(closure (" ++ concat (Data.List.intersperse " " ηs) ++ ") " ++ indentOne ι ε ++ indentOne ι γ ++ ")"
  showIndent _ δ = showData δ

-------------
-- Control --
-------------

type Identifier = String

data Control = LiteralNull
             | LiteralBool Bool
             | LiteralNumber Number
             | LiteralString String
             | Get Identifier
             | If Control Control Control
             | Let Identifier Control Control
             | Lambda [Identifier] Control
             | Apply Control [Control]
             deriving (Eq, Show)

instance ShowIndent Control where
  showIndent _ LiteralNull =  "#n"
  showIndent _ (LiteralBool True) = "#t"
  showIndent _ (LiteralBool False) = "#t"
  showIndent _ (LiteralNumber ν) = show ν
  showIndent _ (LiteralString ν) = escape ν
  showIndent _ (Get η) = η
  showIndent ι (If γ γ' γ'') = "(if" ++ indentOne ι γ ++ indentOne ι γ' ++ indentOne ι γ'' ++ ")"
  showIndent ι (Let η γ γ') = "(let " ++ η ++ indentOne ι γ ++ indentOne ι γ' ++ ")"
  showIndent ι (Lambda ηs γ) = "(lambda (" ++ concat (Data.List.intersperse " " ηs) ++ ")" ++ indentOne ι γ ++ ")"
  showIndent ι (Apply (Get η) γs) = "(" ++ η ++ " " ++ indentAll ι γs ++ ")"
  showIndent ι (Apply γ γs) = "(" ++ indentAll ι (γ : γs) ++ ")"

-----------------
-- Environment --
-----------------

type Environment x = Data.Map.Strict.Map Identifier x

instance (ShowIndent x) => ShowIndent (Environment x) where
  showIndent ι ε = "{" ++ concat (map (\(η, ξ) -> indent (ι + 1) ++ η ++ ": " ++ showIndent (ι + 1) ξ) (Data.Map.Strict.toAscList ε)) ++ "}"

-----------
-- Store --
-----------

type Address = Data.Word.Word32

type Store y = Data.Map.Strict.Map Address y

instance (ShowIndent y) => ShowIndent (Store y) where
  showIndent ι σ = "[" ++ concat (map (\(α, ψ) -> indent (ι + 1) ++ show α ++ " -> " ++ showIndent (ι + 1) ψ) (Data.Map.Strict.toAscList σ)) ++ "]"

------------------
-- Kontinuation --
------------------

data Kontinuation x = LetKont (Environment x) Identifier Control (Kontinuation x)
                    | ApplyKont (Environment x) [Control] [x] (Kontinuation x)
                    | IfKont (Environment x) Control Control (Kontinuation x)
                    | SuccessKont
                    deriving (Eq, Show)

instance (ShowIndent x) => ShowIndent (Kontinuation x) where
  showIndent ι (LetKont ε η γ κ) = "(let-kont " ++ η ++ indentOne ι ε ++ indentOne ι γ ++ indentOne ι κ ++ ")"
  showIndent ι (ApplyKont ε γs ξs κ) = "(apply-kont" ++ indentOne ι ε ++ indentAll ι γs ++ indentAll ι ξs ++ indentOne ι κ ++ ")"
  showIndent ι (IfKont ε γ γ' κ) = "(if-kont " ++ indentOne ι ε ++ indentOne ι γ ++ indentOne ι γ' ++ indentOne ι κ ++ ")"
  showIndent _ SuccessKont = "(success-kont)"

-----------
-- State --
-----------

data State x y = Ongoing Control (Environment x) (Store y) (Kontinuation x)
               | Success (Store y) x
               | Failure String
               deriving (Eq, Show)

instance (ShowIndent x, ShowIndent y) => ShowIndent (State x y) where
  showIndent ι (Ongoing γ ε σ κ) = "(ongoing" ++ indentOne ι γ ++ indentOne ι ε ++ indentOne ι σ ++ indentOne ι κ ++ ")"
  showIndent ι (Success σ ξ) = "(success" ++ indentOne ι ξ ++ indentOne ι σ ++ ")"
  showIndent ι (Failure ν) = "(failure " ++ ν ++ ")"

------------
-- System --
------------

class (Eq x, Eq y) => System x y where
  dataToValue :: Store y -> Data x -> (Store y, x)
  valueToData :: Store y -> x -> Data x
  mutate :: Store y -> x -> Data x -> Maybe (Store y)
  inspect :: Store y -> x -> String

-----------------
-- Interpreter --
-----------------

step :: (System x y) => State x y -> IO (State x y)
step (Ongoing (LiteralNull) _ σ κ) = kontinue κ (dataToValue σ (Null))
step (Ongoing (LiteralBool ν) _ σ κ) = kontinue κ (dataToValue σ (Bool ν))
step (Ongoing (LiteralString ν) _ σ κ) = kontinue κ (dataToValue σ (String ν))
step (Ongoing (LiteralNumber ν) _ σ κ) = kontinue κ (dataToValue σ (Number ν))
step (Ongoing (Lambda ηs γ) ε σ κ) = kontinue κ (dataToValue σ (Closure ε ηs γ))
step (Ongoing (Get η) ε σ κ) = maybe (return $ Failure $ "Undeclared identifier: " ++ η)
                                     (kontinue κ . (σ,))
                                     (Data.Map.Strict.lookup η ε)
step (Ongoing (Let η γ γ') ε σ κ) = return $ Ongoing γ ε σ (LetKont ε η γ' κ)
step (Ongoing (Apply γ γs) ε σ κ) = return $ Ongoing γ ε σ (ApplyKont ε γs [] κ)
step (Ongoing (If γ γ' γ'') ε σ κ) = return $ Ongoing γ ε σ (IfKont ε γ' γ'' κ)
step φ = return φ

kontinue :: (System x y) => Kontinuation x -> (Store y, x) -> IO (State x y)
kontinue SuccessKont (σ, ξ) = return $ Success σ ξ
kontinue (IfKont ε γ γ' κ) (σ, ξ) = case valueToData σ ξ of
  (Number 0) -> return $ Ongoing γ' ε σ κ
  _          -> return $ Ongoing γ  ε σ κ
kontinue (LetKont ε η γ κ) (σ, ξ) = return $ Ongoing γ (Data.Map.Strict.insert η ξ ε) σ κ
kontinue (ApplyKont ε (γ:γs) ξs κ) (σ, ξ) = return $ Ongoing γ ε σ (ApplyKont ε γs (ξs ++ [ξ]) κ)
kontinue (ApplyKont ε [] ξs κ) (σ, ξ) = case valueToData σ (head $ ξs ++ [ξ]) of
  (Closure ε ηs γ) -> return $ Ongoing γ (Data.Map.Strict.union (Data.Map.Strict.fromList (zip ηs (tail ξs ++ [ξ]))) ε) σ κ
  (Builtin β) -> do μ <- (applyBuiltin β (tail $ ξs ++ [ξ]) σ)
                    maybe (return $ Failure $ "Builtin (" ++ β ++ ") application failure") (kontinue κ) μ
  δ -> return $ Failure $ "Cannot apply: " ++ showData δ

applyBuiltin :: (System x y) => String -> [x] -> Store y -> IO (Maybe (Store y, x))
applyBuiltin "read-line" [] σ = Control.Monad.liftM (Just . (dataToValue σ) . String) getLine
applyBuiltin "write" (ξ:[]) σ = case valueToData σ ξ of
  (String ν) -> putStr ν >> (return $ Just (σ, ξ))
  _ -> return Nothing
applyBuiltin β ξs σ = return $ applyPure β ξs σ

applyPure :: (System x y) => String -> [x] -> Store y -> Maybe (Store y, x)
applyPure "eq?" (ξ:ξ':[]) σ = Just $ dataToValue σ (Bool $ ξ == ξ')
applyPure "inspect" (ξ:[]) σ = Just $ dataToValue σ (String $ inspect σ ξ)
applyPure "cons" (ξ:ξ':[]) σ = Just $ dataToValue σ (Cons ξ ξ')
applyPure "car" (ξ:[]) σ = consOnly (valueToData σ ξ) (Just . (σ,) . fst)
applyPure "cdr" (ξ:[]) σ = consOnly (valueToData σ ξ) (Just . (σ,) . snd)
applyPure "set-car!" (ξ:ξ':[]) σ = consOnly (valueToData σ ξ) (\(_,ξ'') -> fmap (,ξ) (mutate σ ξ (Cons ξ' ξ'')))
applyPure "set-cdr!" (ξ:ξ':[]) σ = consOnly (valueToData σ ξ) (\(ξ'',_) -> fmap (,ξ) (mutate σ ξ (Cons ξ'' ξ')))
applyPure β ξs σ = fmap (dataToValue σ) (applyData β (map (valueToData σ) ξs))

applyData :: (Eq x) => String -> [Data x] -> Maybe (Data x)
applyData "equal?" (δ : δ' : []) = Just $ Bool $ δ == δ'
applyData "&&" (Bool ν   : Bool ν'   : []) = Just $ Bool   $ ν && ν'
applyData "||" (Bool ν   : Bool ν'   : []) = Just $ Bool   $ ν || ν'
applyData "="  (Number ν : Number ν' : []) = Just $ Bool   $ ν == ν'
applyData "<"  (Number ν : Number ν' : []) = Just $ Bool   $ ν <  ν'
applyData "<=" (Number ν : Number ν' : []) = Just $ Bool   $ ν <= ν'
applyData "+"  (Number ν : Number ν' : []) = Just $ Number $ ν +  ν'
applyData "-"  (Number ν : Number ν' : []) = Just $ Number $ ν -  ν'
applyData "*"  (Number ν : Number ν' : []) = Just $ Number $ ν *  ν'
applyData "/"  (Number ν : Number ν' : []) = Just $ Number $ ν /  ν'
applyData "any->string" (δ : []) = Just $ String $ showData δ
applyData "string-append" (String ν : String ν' : []) = Just $ String $ ν ++ ν'
applyData "string-length" (String ν : []) = Just $ Number $ fromIntegral $ length ν
applyData "substring" (String ν : Number ν' : Number ν'' : []) = Just $ String $ take (round $ ν'' - ν') (drop (round ν') ν)
applyData "number->string" (Number ν : []) = Just $ String $ show ν
applyData "string->number" (String ν : []) = case reads ν of
  [(ν', "")] -> Just $ Number ν'
  _ -> Nothing
applyData _ _  = Nothing

-------------
-- Helpers --
-------------

fresh :: Store y -> Address
fresh σ = maybe 0 ((+1) . fst) (Data.Map.Strict.lookupMax σ)

reuse :: (Eq y) => Store y -> y -> Address
reuse σ ψ = maybe (fresh σ)
                  id
                  (Data.Map.Strict.foldrWithKey (\α ψ' μ -> if ψ == ψ' then Just α else μ) Nothing σ)

consOnly :: Data x -> ((x, x) -> Maybe z) -> Maybe z
consOnly (Cons ξ ξ') f = f (ξ, ξ')
consOnly _ _ = Nothing

------------
-- Parser --
------------

lexer :: Text.Parsec.Token.TokenParser u
lexer = Text.Parsec.Token.makeTokenParser $ Text.Parsec.Token.LanguageDef {
  Text.Parsec.Token.commentStart = "",
  Text.Parsec.Token.commentEnd = "",
  Text.Parsec.Token.commentLine = ";",
  Text.Parsec.Token.nestedComments = False,
  Text.Parsec.Token.identStart = Text.Parsec.noneOf "() \t\n#\"",
  Text.Parsec.Token.identLetter = Text.Parsec.noneOf "() \t\n#\"",
  Text.Parsec.Token.opStart = Text.Parsec.oneOf [],
  Text.Parsec.Token.opLetter = Text.Parsec.oneOf [],
  Text.Parsec.Token.reservedNames = ["if", "let", "lambda", "#n", "#t", "#f"],
  Text.Parsec.Token.reservedOpNames = [],
  Text.Parsec.Token.caseSensitive = True
}

parserIdentifier = Text.Parsec.Token.identifier lexer
parserWhiteSpace = Text.Parsec.Token.whiteSpace lexer
parserStringLiteral = Text.Parsec.Token.stringLiteral lexer
parserInteger = Text.Parsec.Token.integer lexer
parserParens = Text.Parsec.Token.parens lexer
parserReserved = Text.Parsec.Token.reserved lexer

parserControl :: Text.Parsec.Parsec String u Control
parserControl = Text.Parsec.choice [
  Text.Parsec.try parserLiteralNull,
  Text.Parsec.try parserLiteralBool,
  Text.Parsec.try parserLiteralNumber,
  Text.Parsec.try parserLiteralString,
  Text.Parsec.try parserGet,
  Text.Parsec.try parserIf,
  Text.Parsec.try parserLet,
  Text.Parsec.try parserLambda,
  parserApply]

parserLiteralNull :: Text.Parsec.Parsec String u Control
parserLiteralNull = parserReserved "#n" >> return LiteralNull

parserLiteralBool :: Text.Parsec.Parsec String u Control
parserLiteralBool = (Text.Parsec.<|>) (parserReserved "#t" >> (return $ LiteralBool True))
                                      (parserReserved "#f" >> (return $ LiteralBool False))

parserLiteralNumber :: Text.Parsec.Parsec String u Control
parserLiteralNumber = Control.Monad.liftM (LiteralNumber . fromInteger) parserInteger

parserLiteralString :: Text.Parsec.Parsec String u Control
parserLiteralString = Control.Monad.liftM LiteralString parserStringLiteral

parserGet :: Text.Parsec.Parsec String u Control
parserGet = Control.Monad.liftM Get parserIdentifier

parserIf :: Text.Parsec.Parsec String u Control
parserIf = parserParens $ do parserReserved "if"
                             γ <- parserControl
                             γ' <- parserControl
                             γ'' <- parserControl
                             return $ If γ γ' γ''

parserLet :: Text.Parsec.Parsec String u Control
parserLet = parserParens $ do parserReserved "let"
                              η <- parserIdentifier
                              γ <- parserControl
                              γ' <- parserControl
                              return $ Let η γ γ'

parserLambda :: Text.Parsec.Parsec String u Control
parserLambda = parserParens $ do parserReserved "lambda"
                                 ηs <- parserParens (Text.Parsec.many parserIdentifier)
                                 γ <- parserControl
                                 return $ Lambda ηs γ

parserApply :: Text.Parsec.Parsec String u Control
parserApply = parserParens $ do γ <- parserControl
                                γs <- Text.Parsec.many parserControl
                                return $ Apply γ γs

-------------------
-- System Stored --
-------------------

data ValueStored = AddressStored Address deriving (Eq, Show)
data ElementStored = DataStored (Data ValueStored) deriving (Eq, Show)

instance System ValueStored ElementStored where
  dataToValue σ δ = (Data.Map.Strict.insert (fresh σ) (DataStored δ) σ, AddressStored (fresh σ))
  valueToData σ (AddressStored α) = let (DataStored δ) = σ Data.Map.Strict.! α in δ
  mutate σ (AddressStored α) δ = Just $ Data.Map.Strict.insert α (DataStored δ) σ
  inspect σ (AddressStored α) = "&" ++ show α

instance ShowIndent ValueStored where
  showIndent _ (AddressStored α) = "&" ++ show α

instance ShowIndent ElementStored where
  showIndent ι (DataStored δ) = showIndent ι δ

-------------------------
-- System StoredReuse  --
-------------------------

data ValueStoredReuse = AddressStoredReuse Address deriving (Eq, Show)
data ElementStoredReuse = DataStoredReuse (Data ValueStoredReuse) deriving (Eq, Show)

instance System ValueStoredReuse ElementStoredReuse where
  dataToValue σ δ = (Data.Map.Strict.insert (reuse σ (DataStoredReuse δ)) (DataStoredReuse δ) σ, AddressStoredReuse (fresh σ))
  valueToData σ (AddressStoredReuse α) = let (DataStoredReuse δ) = σ Data.Map.Strict.! α in δ
  mutate σ (AddressStoredReuse α) δ = Just $ Data.Map.Strict.insert α (DataStoredReuse δ) σ
  inspect σ (AddressStoredReuse α) = "&" ++ show α

instance ShowIndent ValueStoredReuse where
  showIndent _ (AddressStoredReuse α) = "&" ++ show α

instance ShowIndent ElementStoredReuse where
  showIndent ι (DataStoredReuse δ) = showIndent ι δ

-------------------
-- System Inline --
-------------------

data ValueInline = DataInline (Data ValueInline) deriving (Eq, Show)
data ElementInline = Void deriving (Eq, Show)

instance System ValueInline ElementInline where
  dataToValue σ δ = (σ, DataInline δ)
  valueToData _ (DataInline δ) = δ
  mutate _ _ _ = Nothing
  inspect _ (DataInline δ) = showData δ

instance ShowIndent ValueInline where
  showIndent ι (DataInline δ) = showIndent ι δ

instance ShowIndent ElementInline where
  showIndent _ (Void) = error "ElementInline should never be instantiated"

------------------
-- System Mixed --
------------------

data ValueMixed = NullMixed
                | BoolMixed Bool
                | NumberMixed Number
                | StringMixed String
                | BuiltinMixed Builtin
                | ClosureMixed (Environment ValueMixed) [Identifier] Control
                | AddressMixed Address
                deriving (Eq, Show)

data ElementMixed = ConsMixed ValueMixed ValueMixed deriving (Eq, Show)

instance System ValueMixed ElementMixed where
  dataToValue σ Null = (σ, NullMixed)
  dataToValue σ (Bool ν) = (σ, BoolMixed ν)
  dataToValue σ (Number ν) = (σ, NumberMixed ν)
  dataToValue σ (String ν) = (σ, StringMixed ν)
  dataToValue σ (Builtin ν) = (σ, BuiltinMixed ν)
  dataToValue σ (Closure ε ηs γ) = (σ, ClosureMixed ε ηs γ)
  dataToValue σ (Cons ξ ξ') = (Data.Map.Strict.insert (fresh σ) (ConsMixed ξ ξ') σ, AddressMixed $ fresh σ)
  valueToData σ NullMixed = Null
  valueToData σ (BoolMixed ν) = Bool ν
  valueToData σ (NumberMixed ν) = Number ν
  valueToData σ (StringMixed ν) = String ν
  valueToData σ (BuiltinMixed ν) = Builtin ν
  valueToData σ (ClosureMixed ε ηs γ) = Closure ε ηs γ
  valueToData σ (AddressMixed α) = case σ Data.Map.Strict.! α of (ConsMixed ξ ξ') -> Cons ξ ξ'
  mutate σ (AddressMixed α) (Cons ξ ξ') = Just $ Data.Map.Strict.insert α (ConsMixed ξ ξ') σ
  mutate _ _ _ = Nothing
  inspect σ (NullMixed) = showData Null
  inspect σ (BoolMixed ν) = showData $ Bool ν
  inspect σ (NumberMixed ν) = showData $ Number ν
  inspect σ (StringMixed ν) = showData $ String ν
  inspect σ (BuiltinMixed ν) = showData $ Builtin ν
  inspect σ (ClosureMixed ε ηs γ) = showData $ Closure ε ηs γ
  inspect σ (AddressMixed α) = "&" ++ show α

instance ShowIndent ValueMixed where
  showIndent ι (NullMixed) = showIndent ι (Null :: Data ValueMixed)
  showIndent ι (BoolMixed ν) = showIndent ι (Bool ν :: Data ValueMixed)
  showIndent ι (NumberMixed ν) = showIndent ι (Number ν :: Data ValueMixed)
  showIndent ι (StringMixed ν) = showIndent ι (String ν :: Data ValueMixed)
  showIndent ι (BuiltinMixed ν) = showIndent ι (Builtin ν :: Data ValueMixed)
  showIndent ι (ClosureMixed ε ηs γ) = showIndent ι (Closure ε ηs γ :: Data ValueMixed)
  showIndent ι (AddressMixed α) = "&" ++ show α

instance ShowIndent ElementMixed where
  showIndent ι (ConsMixed ξ ξ') = showIndent ι (Cons ξ ξ')

----------
-- Main --
----------

builtins :: [Builtin]
builtins = [
  "read-line",
  "write",
  "eq?",
  "inspect",
  "cons",
  "car",
  "cdr",
  "set-car!",
  "set-cdr!",
  "equal?",
  "||",
  "&&",
  "=",
  "<=",
  "+",
  "-",
  "*",
  "/",
  "any->string",
  "number->string",
  "string->number",
  "string-append",
  "substring"]

execute :: (System x y) => Control -> IO (State x y)
execute γ = let (σ, ε) = Data.List.foldl' accumulator (Data.Map.Strict.empty, Data.Map.Strict.empty) builtins
            in run $ Ongoing γ ε σ SuccessKont
  where accumulator (σ, ε) β = let (σ', ξ) = dataToValue σ (Builtin β)
                               in (σ', Data.Map.Strict.insert β ξ ε)

executeSystem :: String -> Control -> IO String
executeSystem "inline" γ = Control.Monad.liftM (showIndent 0) (execute γ :: IO (State ValueInline ElementInline))
executeSystem "mixed" γ = Control.Monad.liftM (showIndent 0) (execute γ :: IO (State ValueMixed ElementMixed))
executeSystem "stored" γ = Control.Monad.liftM (showIndent 0) (execute γ :: IO (State ValueStored ElementStored))
executeSystem "stored-reuse" γ = Control.Monad.liftM (showIndent 0) (execute γ :: IO (State ValueStoredReuse ElementStoredReuse))
executeSystem λ γ = return $ "Unknown system: " ++ λ

top :: String -> String -> IO String
top λ ρ = Text.Parsec.String.parseFromFile parserControl ρ >>= (either (return . show) (executeSystem λ))

run :: (System x y) => State x y -> IO (State x y)
run φ@(Ongoing _ _ _ _) = step φ >>= run
run φ = return φ

main :: IO ()
main = do ξs <- System.Environment.getArgs
          if length ξs /= 2
          then putStrLn "Usage: cesk (inline|mixed|stored|stored-reuse) program.lsp"
          else top (head ξs) (head $ tail ξs) >>= putStrLn
