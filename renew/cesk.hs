
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}

import qualified Data.Map
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

showIndentOne :: ShowIndent a => Int -> a -> String
showIndentOne ι χ = indent (ι + 1) ++ showIndent (ι + 1) χ

showIndentAll :: ShowIndent a => Int -> [a] -> String
showIndentAll ι χs = (concat $ map (showIndentOne ι) χs)

showIndentAllBracket :: ShowIndent a => Int -> [a] -> String
showIndentAllBracket ι χs = indent (ι + 1) ++ "[" ++ showIndentAll (ι + 1) χs ++ "]"

----------
-- Data --
----------

type Boolean = Bool

type Number = Float

type Builtin = String

type Cons v = (v, v)

type Closure v = (Environment v, [Identifier], Control)

data Atomic = Null
            | Boolean Boolean
            | Number Number
            | String String
            | Builtin Builtin

instance Eq Atomic where
  Null == Null = True
  (Boolean ο) == (Boolean ο') = ο == ο'
  (Number ν) == (Number ν') = ν == ν' || (ν /= ν && ν' /= ν')
  (String τ) == (String τ') = τ == τ'
  (Builtin β) == (Builtin β') = β == β'
  _ == _ = False

data Compound v = Cons (Cons v)
                | Closure (Closure v)

data Data v = Atomic Atomic
            | Compound (Compound v)

toAtomic :: Data x -> Maybe Atomic
toAtomic (Atomic θ) = Just θ
toAtomic _ = Nothing

toCompound :: Data x -> Maybe (Compound x)
toCompound (Compound π) = Just π
toCompound _ = Nothing

isNull :: Data x -> Bool
isNull (Atomic Null) = True
isNull _ = False

toBoolean :: Data x -> Maybe Boolean
toBoolean (Atomic (Boolean ο)) = Just ο
toBoolean _ = Nothing

toNumber :: Data x -> Maybe Number
toNumber (Atomic (Number ν)) = Just ν
toNumber _ = Nothing

toString :: Data x -> Maybe String
toString (Atomic (String τ)) = Just τ
toString _ = Nothing

toBuiltin :: Data x -> Maybe Builtin
toBuiltin (Atomic (Builtin β)) = Just β
toBuiltin _ = Nothing

toCons :: Data x -> Maybe (Cons x)
toCons (Compound (Cons (ξ, ξ'))) = Just (ξ, ξ')
toCons _ = Nothing

toClosure :: Data x -> Maybe (Closure x) 
toClosure (Compound (Closure (ε, ηs, γ))) = Just (ε, ηs, γ)
toClosure _ = Nothing

showAtomic :: Atomic -> String
showAtomic (Null) = "#n"
showAtomic (Boolean True) = "#t"
showAtomic (Boolean False) = "#f"
showAtomic (Number ν) = if ν == fromInteger (round ν) then show (round ν) else show ν
showAtomic (String τ) = escape τ
  where escape ν = "\"" ++ concat (map repl ν) ++ "\""
        repl '\n' = "\\n"
        repl '\t' = "\\t"
        repl '"' = "\\\""
        repl ν = [ν]
showAtomic (Builtin β) = "<#" ++ β ++ ">"

showCompound :: Compound v -> String
showCompound (Cons _) = "<Cons>"
showCompound (Closure _) = "<Closure>"

showData :: Data v -> String
showData (Atomic θ) = showAtomic θ
showData (Compound π) = showCompound π

instance ShowIndent Atomic where
  showIndent _ θ = showAtomic θ

instance (ShowIndent v) => ShowIndent (Compound v) where
  showIndent ι (Cons (ξ, ξ')) = "(cons" ++ showIndentOne ι ξ ++ showIndentOne ι ξ' ++ ")"
  showIndent ι (Closure (ε, ηs, γ)) = "(closure (" ++ concat (Data.List.intersperse " " ηs) ++ ") " ++ showIndentOne ι ε ++ showIndentOne ι γ ++ ")"

instance (ShowIndent v) => ShowIndent (Data v) where
  showIndent ι (Atomic θ) = showIndent ι θ
  showIndent ι (Compound π) = showIndent ι π

-------------
-- Control --
-------------

type Identifier = String

data Control = Literal Atomic
             | Variable Identifier
             | Condition Control Control Control
             | Binding Identifier Control Control
             | Abstraction [Identifier] Control
             | Application Control [Control]

instance ShowIndent Control where
  showIndent ι (Literal θ) = showIndent ι θ
  showIndent _ (Variable η) = η
  showIndent ι (Condition γ γ' γ'') = "(if" ++ showIndentOne ι γ ++ showIndentOne ι γ' ++ showIndentOne ι γ'' ++ ")"
  showIndent ι (Binding η γ γ') = "(let " ++ η ++ showIndentOne ι γ ++ showIndentOne ι γ' ++ ")"
  showIndent ι (Abstraction ηs γ) = "(lambda (" ++ concat (Data.List.intersperse " " ηs) ++ ")" ++ showIndentOne ι γ ++ ")"
  showIndent ι (Application γ γs) = "(" ++ showIndentAll ι (γ : γs) ++ ")"

lexer :: Text.Parsec.Token.TokenParser u
lexer = Text.Parsec.Token.makeTokenParser $ Text.Parsec.Token.LanguageDef {
  Text.Parsec.Token.commentStart = "",
  Text.Parsec.Token.commentEnd = "",
  Text.Parsec.Token.commentLine = ";",
  Text.Parsec.Token.nestedComments = False,
  Text.Parsec.Token.identStart = Text.Parsec.noneOf "() \t\n",
  Text.Parsec.Token.identLetter = Text.Parsec.noneOf "() \t\n",
  Text.Parsec.Token.opStart = Text.Parsec.oneOf [],
  Text.Parsec.Token.opLetter = Text.Parsec.oneOf [],
  Text.Parsec.Token.reservedNames = ["if", "let", "lambda", "#n", "#t", "#f"],
  Text.Parsec.Token.reservedOpNames = [],
  Text.Parsec.Token.caseSensitive = True
}

parserIdentifier = Text.Parsec.Token.identifier lexer

parserParens = Text.Parsec.Token.parens lexer

parserReserved = Text.Parsec.Token.reserved lexer

parserAtomic = Text.Parsec.choice [Text.Parsec.try parserNull, Text.Parsec.try parserBoolean, Text.Parsec.try parserNumber, Text.Parsec.try parserString]
  where parserNull = parserReserved "#n" >> (return Null)
        parserBoolean = (Text.Parsec.<|>) (parserReserved "#t" >> (return $ Boolean True))
                                          (parserReserved "#f" >> (return $ Boolean False))
        parserNumber = fmap (Number . either fromInteger realToFrac) (Text.Parsec.Token.naturalOrFloat lexer)
        parserString = fmap String (Text.Parsec.Token.stringLiteral lexer)

meta :: Identifier -> Text.Parsec.SourcePos -> [Control] -> Control
meta η π γs = Application (Variable $ "meta-" ++ η) (γs ++ [Literal $ String $ show (Text.Parsec.sourceLine π) ++ ":" ++ show (Text.Parsec.sourceColumn π)])

-- meta :: String -> [Control] -> Control
-- meta τ γs = Application (Variable $ "meta-" ++ τ) γs
-- 
-- instrument :: Control -> Control
-- instrument (Literal θ) = meta "literal" [Literal θ]
-- instrument (Variable η) = meta "variable" [Literal $ String η, Variable η]
-- instrument (Condition γ γ' γ'') = Condition (meta "condition" [instrument γ]) (instrument γ') (instrument γ'')
-- instrument (Application γ γs) = meta "application" [instrument γ, Application (Variable "list") (map instrument γs)]
-- instrument (Binding η γ γ') = Binding η (meta "binding" [Literal $ String η, instrument γ]) (instrument γ')

parserLiteral :: String -> Text.Parsec.Parsec String u Control
parserLiteral τ
  | not $ Data.List.isInfixOf "literal" τ = fmap Literal parserAtomic
  | otherwise = do π <- Text.Parsec.getPosition
                   θ <- parserAtomic
                   return $ meta "literal" π [Literal θ]

parserVariable :: String -> Text.Parsec.Parsec String u Control
parserVariable τ
 | not $ Data.List.isInfixOf "variable" τ = fmap Variable parserIdentifier
 | otherwise = do π <- Text.Parsec.getPosition
                  η <- parserIdentifier
                  return $ meta "variable" π [Literal $ String η, Variable η]

parserBinding :: String -> Text.Parsec.Parsec String u Control
parserBinding τ
  | not $ Data.List.isInfixOf "binding" τ = parserParens $ parserReserved "let" >> Control.Monad.liftM3 Binding parserIdentifier (parser τ) (parser τ)
  | otherwise = do π <- Text.Parsec.getPosition
                   parserParens $ do parserReserved "let"
                                     η <- parserIdentifier
                                     γ <- parser τ
                                     γ' <- parser τ
                                     return $ Binding η (meta "binding" π [Literal $ String η, γ]) γ'

parserCondition :: String -> Text.Parsec.Parsec String u Control
parserCondition τ
  | not $ Data.List.isInfixOf "condition" τ = parserParens $ parserReserved "if" >> Control.Monad.liftM3 Condition (parser τ) (parser τ) (parser τ)
  | otherwise = do π <- Text.Parsec.getPosition
                   parserParens $ do parserReserved "if"
                                     γ <- parser τ
                                     γ' <- parser τ
                                     γ'' <- parser τ
                                     return $ Condition (meta "condition" π [γ]) γ' γ''

parserAbstraction :: String -> Text.Parsec.Parsec String u Control
parserAbstraction τ
  | not $ Data.List.isInfixOf "abstraction" τ = parserParens $ parserReserved "lambda" >> Control.Monad.liftM2 Abstraction (parserParens (Text.Parsec.many parserIdentifier)) (parser τ)
  | otherwise = do π <- Text.Parsec.getPosition
                   parserParens $ do parserReserved "lambda"
                                     ηs <- parserParens $ Text.Parsec.many parserIdentifier
                                     γ <- parser τ
                                     return $ meta "abstraction" π [Application (Variable "list") (map (Literal . String) ηs), Abstraction ηs γ]

parserApplication :: String -> Text.Parsec.Parsec String u Control
parserApplication τ
  | not $  Data.List.isInfixOf "application" τ = parserParens $ Control.Monad.liftM2 Application (parser τ) (Text.Parsec.many $ parser τ)
  | otherwise = do π <- Text.Parsec.getPosition
                   parserParens $ do γ <- parser τ
                                     γs <- Text.Parsec.many $ parser τ
                                     return $ meta "application" π [γ, Application (Variable "list") γs]

parser :: String -> Text.Parsec.Parsec String u Control
parser τ = Text.Parsec.choice [
  Text.Parsec.try $ parserLiteral τ,
  Text.Parsec.try $ parserVariable τ,
  Text.Parsec.try $ parserBinding τ,
  Text.Parsec.try $ parserCondition τ,
  Text.Parsec.try $ parserAbstraction τ,
  Text.Parsec.try $ parserApplication τ]

-----------------
-- Environment --
-----------------

type Environment v = Data.Map.Map Identifier v

instance (ShowIndent v) => ShowIndent (Environment v) where
  showIndent ι ε = "{" ++ concat (map (\(η, ξ) -> indent (ι + 1) ++ η ++ ": " ++ showIndent (ι + 1) ξ) (Data.Map.toAscList ε)) ++ "}"

-----------
-- Store --
-----------

type Address = Data.Word.Word32

type Store e = Data.Map.Map Address e

instance (ShowIndent e) => ShowIndent (Store e) where
  showIndent ι σ = "[" ++ concat (map (\(α, ψ) -> indent (ι + 1) ++ show α ++ " -> " ++ showIndent (ι + 1) ψ) (Data.Map.toAscList σ)) ++ "]"

------------------
-- Kontinuation --
------------------

data Kontinuation v = Bind (Environment v) Identifier Control (Kontinuation v)
                    | Apply (Environment v) [Control] [v] (Kontinuation v)
                    | Branch (Environment v) Control Control (Kontinuation v)
                    | Finish

instance (ShowIndent v) => ShowIndent (Kontinuation v) where
  showIndent ι (Bind ε η γ κ) = "(bind " ++ η ++ showIndentOne ι ε ++ showIndentOne ι γ ++ showIndentOne ι κ ++ ")"
  showIndent ι (Apply ε γs ξs κ) = "(apply" ++ showIndentOne ι ε ++ showIndentAllBracket ι γs ++ showIndentAllBracket ι ξs ++ showIndentOne ι κ ++ ")"
  showIndent ι (Branch ε γ γ' κ) = "(branch " ++ showIndentOne ι ε ++ showIndentOne ι γ ++ showIndentOne ι γ' ++ showIndentOne ι κ ++ ")"
  showIndent _ Finish = "(finish)"

-----------
-- State --
-----------

data State v e = Ongoing Control (Environment v) (Store e) (Kontinuation v)
               | Success (Store e) v
               | Failure (Store e) (Kontinuation v) (String, String)

instance (ShowIndent v, ShowIndent e) => ShowIndent (State v e) where
  showIndent ι (Ongoing γ ε σ κ) = "(ongoing" ++ showIndentOne ι γ ++ showIndentOne ι ε ++ showIndentOne ι σ ++ showIndentOne ι κ ++ ")"
  showIndent ι (Success σ ξ) = "(success" ++ showIndentOne ι ξ ++ showIndentOne ι σ ++ ")"
  showIndent ι (Failure σ κ (τ,τ')) = "(failure " ++ τ ++ " >> " ++ τ' ++ showIndentOne ι σ ++ showIndentOne ι κ ++ ")"

------------
-- System --
------------

class (Eq v) => System v e | e -> v where
  dataToValue :: Store e -> Data v -> (Store e, v)
  valueToData :: Store e -> v -> Data v
  mutate :: Store e -> v -> Data v -> Either String (Store e)
  inspect :: Store e -> v -> String
  initialStore :: Store e
  initialStore = Data.Map.empty

-----------------
-- Interpreter --
-----------------

step :: (System v e) => State v e -> IO (State v e)
step (Ongoing (Literal θ) _ σ κ) = kontinue κ (dataToValue σ (Atomic θ))
step (Ongoing (Abstraction ηs γ) ε σ κ) = kontinue κ (dataToValue σ (Compound $ Closure (ε, ηs, γ)))
step (Ongoing (Variable η) ε σ κ) = maybe (return $ Failure σ κ ("ReferenceError", η ++ " is not defined")) (kontinue κ . (σ,)) (Data.Map.lookup η ε)
step (Ongoing (Binding η γ γ') ε σ κ) = return $ Ongoing γ ε σ (Bind ε η γ' κ)
step (Ongoing (Application γ γs) ε σ κ) = return $ Ongoing γ ε σ (Apply ε γs [] κ)
step (Ongoing (Condition γ γ' γ'') ε σ κ) = return $ Ongoing γ ε σ (Branch ε γ' γ'' κ)
step φ = return φ

kontinue :: (System v e) => Kontinuation v -> (Store e, v) -> IO (State v e)
kontinue Finish (σ, ξ) = return $ Success σ ξ
kontinue (Branch ε γ γ' κ) (σ, ξ) = case valueToData σ ξ of
  (Atomic (Boolean (False))) -> return $ Ongoing γ' ε σ κ
  _ -> return $ Ongoing γ  ε σ κ
kontinue (Bind ε η γ κ) (σ, ξ) = return $ Ongoing γ (Data.Map.insert η ξ ε) σ κ
kontinue (Apply ε (γ:γs) ξs κ) (σ, ξ) = return $ Ongoing γ ε σ (Apply ε γs (ξs ++ [ξ]) κ)
kontinue (Apply ε [] ξs κ) (σ, ξ) = apply σ κ (valueToData σ (head $ ξs ++ [ξ])) (tail $ ξs ++ [ξ]) 

apply :: (System v e) => Store e -> Kontinuation v -> Data v -> [v] -> IO (State v e)
apply σ κ δ@(Compound (Closure (ε, ηs, γ))) ξs = return $ if length ηs == length ξs
  then Ongoing γ (Data.Map.union (Data.Map.fromList (zip ηs ξs)) ε) σ κ
  else Failure σ κ ("ArityError", showData δ ++  " expected " ++ show (length ηs) ++ " argument(s) but received " ++ show (length ξs)) 
apply σ κ (Atomic (Builtin β)) ξs = fmap (either (Failure σ κ . ("BuiltinError",) . format) id) (applyBuiltin σ κ β ξs)
  where format τ = (showAtomic $ Builtin β) ++ ": " ++ τ ++ ", got: " ++ concat (Data.List.intersperse ", " (map (inspect σ) ξs))
apply σ κ δ _ = return $ Failure σ κ ("TypeError", showData δ ++ " cannot be applied")

applyBuiltin :: (System x y) => Store y -> Kontinuation x -> Builtin -> [x] -> IO (Either String (State x y))
applyBuiltin σ κ "apply" (ξ:ξ':[]) = maybe (return $ Left "did not received a list as second argument")
                                           (fmap Right . apply σ κ (valueToData σ ξ))
                                           (loop $ valueToData σ ξ')
  where loop (Compound (Cons (ξ'', ξ'''))) = fmap (ξ'':) (loop $ valueToData σ ξ''')
        loop (Atomic Null) = Just []
        loop _ = Nothing
applyBuiltin σ κ "read-line" [] = getLine >>= (fmap Right . kontinue κ . (dataToValue σ) . Atomic . String)
applyBuiltin σ κ "print" ξs@(_:_) = putStrLn (loop (map (valueToData σ) ξs)) >> fmap Right (kontinue κ (dataToValue σ (Atomic Null)))
  where loop ((Atomic (String τ)) : δs) = τ ++ loop δs
        loop (δ : δs) = showData δ ++ loop δs
        loop [] = ""
applyBuiltin σ κ β ξs = either (return . Left) (fmap Right . kontinue κ) (applyPure σ β ξs)

applyPure :: (System v e) => Store e -> Builtin -> [v] -> Either String (Store e, v)
applyPure σ "begin" ξs@(_:_) = Right (σ, last ξs)
applyPure σ "eq?" (ξ:ξ':[]) = Right $ dataToValue σ (Atomic $ Boolean $ ξ == ξ')
applyPure σ "inspect" ξs = Right $ dataToValue σ (Atomic $ String $ concat $ Data.List.intersperse ", " (map (inspect σ) ξs))
applyPure σ "cons" (ξ:ξ':[]) = Right $ dataToValue σ (Compound $ Cons (ξ, ξ'))
applyPure σ "list" ξs = Right $ foldr (\ξ (σ, ξ') -> dataToValue σ (Compound $ Cons (ξ, ξ'))) (dataToValue σ (Atomic $ Null)) ξs
applyPure σ "car" (ξ:[]) = maybe (Left "expected a pair as first argument")
                                 (Right . (σ,) . fst)
                                 (toCons $ valueToData σ ξ)
applyPure σ "cdr" (ξ:[]) = maybe (Left "expected a pair as first argument")
                                 (Right . (σ,) . snd)
                                 (toCons $ valueToData σ ξ)
applyPure σ "set-car!" (ξ:ξ':[]) = maybe (Left "expected a pair as first argument")
                                         (fmap (,ξ) . mutate σ ξ . Compound . Cons . (ξ',) . snd)
                                         (toCons $ valueToData σ ξ)
applyPure σ "set-cdr!" (ξ:ξ':[]) = maybe (Left "expected a pair as first argument")
                                         (fmap (,ξ) . mutate σ ξ . Compound . Cons . (,ξ') . fst)
                                         (toCons $ valueToData σ ξ)
applyPure σ β ξs = fmap (dataToValue σ) (applyData β (map (valueToData σ) ξs))

applyData :: (Eq v) => Builtin -> [Data v] -> Either String (Data v)
applyData "raise" (δ : []) = Left $ showData δ
applyData "any->string" (δ : []) = Right $ Atomic $ String $ showData δ
applyData "null?" (Atomic (Null) : []) = Right $ Atomic $ Boolean True
applyData "null?" (_ : []) = Right $ Atomic $ Boolean False
applyData "boolean?" (Atomic (Boolean _) : []) = Right $ Atomic $ Boolean True
applyData "boolean?" (_ : []) = Right $ Atomic $ Boolean False
applyData "number?" (Atomic (Number _) : []) = Right $ Atomic $ Boolean True
applyData "number?" (_ : []) = Right $ Atomic $ Boolean False
applyData "string?" (Atomic (String _) : []) = Right $ Atomic $ Boolean True
applyData "string?" (_ : []) = Right $ Atomic $ Boolean False
applyData "pair?" (Compound (Cons _) : []) = Right $ Atomic $ Boolean True
applyData "pair?" (_ : []) = Right $ Atomic $ Boolean False
applyData "procedure?" (Compound (Closure _) : []) = Right $ Atomic $ Boolean True
applyData "procedure?" (Atomic (Builtin _) : []) = Right $ Atomic $ Boolean True
applyData "procedure?" (_ : []) = Right $ Atomic $ Boolean False
applyData β δs = maybe (Left $ "expected only atomic arguments")
                       (fmap Atomic . applyAtomic β)
                       (sequence $ map toAtomic δs)

applyAtomic :: String -> [Atomic] -> Either String Atomic
applyAtomic "eqv?" (θ : θ' : []) = Right $ Boolean $ θ == θ'
applyAtomic "&&"   (Boolean ο : Boolean ο' : []) = Right $ Boolean $ ο && ο'
applyAtomic "||"   (Boolean ο : Boolean ο' : []) = Right $ Boolean $ ο || ο'
applyAtomic "="    (Number  ν : Number  ν' : []) = Right $ Boolean $ ν == ν'
applyAtomic "<"    (Number  ν : Number  ν' : []) = Right $ Boolean $ ν <  ν'
applyAtomic "<="   (Number  ν : Number  ν' : []) = Right $ Boolean $ ν <= ν'
applyAtomic "+"    θs = maybe (Left "expected only numbers")
                              (Right . Number . foldr (+) 0)
                              (sequence $ map (toNumber . Atomic) θs)
applyAtomic "-"    (Number  ν : Number  ν' : []) = Right $ Number  $ ν -  ν'
applyAtomic "-"    (Number  ν : []) = Right $ Number  $ 0 - ν
applyAtomic "*"    θs = maybe (Left "expected only numbers")
                              (Right . Number . foldr (*) 1)
                              (sequence $ map (toNumber . Atomic) θs)
applyAtomic "/"    (Number  ν : Number  ν' : []) = Right $ Number  $ ν /  ν'
applyAtomic "expt" (Number  ν : Number  ν' : []) = Right $ Number  $ ν ** ν'
applyAtomic "sqrt" (Number  ν : []) = Right $ Number  $ sqrt ν
applyAtomic "string-append" θs = maybe (Left "expected only strings")
                                       (Right . String . concat)
                                       (sequence $ map (toString . Atomic) θs)
applyAtomic "string-length" (String τ : []) = Right $ Number $ fromIntegral $ length τ
applyAtomic "substring" (String τ : Number ν : Number ν' : []) = Right $ String $ take (round $ ν' - ν) (drop (round ν) τ)
applyAtomic "number->string" (Number ν : []) = Right $ String $ showAtomic $ Number ν
applyAtomic "string->number" (String τ : []) = case reads τ of
  [(ν, "")] -> Right $ Number ν
  _ -> Left "cannot parse as number"
applyAtomic β θs = Left $ if elem β builtins then "invalid arguments" else "unsupported builtin"

---------------------------------------
-- System1: Data Passed-By-Reference --
---------------------------------------

data Value1 = Address1 Address

instance Eq Value1 where
  (Address1 α) == (Address1 α') = α == α'

data Element1 = Data1 (Data Value1)

fresh :: Store e -> Address
fresh σ = maybe 0 ((+1) . fst) (Data.Map.lookupMax σ)

instance System Value1 Element1 where
  dataToValue σ δ = (σ', Address1 α)
    where α = fresh σ
          σ' = Data.Map.insert α (Data1 δ) σ
  valueToData σ (Address1 α) = δ where
    (Data1 δ) = σ Data.Map.! α
  mutate σ (Address1 α) δ = Right σ'
    where σ' = Data.Map.insert α (Data1 δ) σ
  inspect σ (Address1 α) = "&" ++ show α ++ "[" ++ showData δ ++ "]"
    where (Data1 δ) = σ Data.Map.! α

instance ShowIndent Value1 where
  showIndent _ (Address1 α) = "&" ++ show α

instance ShowIndent Element1 where
  showIndent ι (Data1 δ) = showIndent ι δ

  -------------------------------------------------------------------------------
  -- System2: Atomic Data Passed-By-Value & Compound Data Passed-By-Reference  --
  -------------------------------------------------------------------------------

data Value2 = Atomic2 Atomic
            | Address2 Address

instance Eq Value2 where
  (Atomic2 θ) == (Atomic2 θ') = θ == θ'
  (Address2 α) == (Address2 α') = α == α'

data Element2 = Compound2 (Compound Value2)

instance System Value2 Element2 where
  dataToValue σ (Atomic θ) = (σ, Atomic2 θ)
  dataToValue σ (Compound π) = (σ', Address2 α)
    where α = fresh σ
          σ' = Data.Map.insert α (Compound2 π) σ
  valueToData σ (Atomic2 θ) = Atomic θ
  valueToData σ (Address2 α) = Compound π
    where (Compound2 π) = σ Data.Map.! α
  mutate σ (Address2 α) (Compound π) = Right σ'
    where σ' = Data.Map.insert α (Compound2 π) σ
  mutate _ _ _ = Left "Cannot mutate atomic data"
  inspect σ (Atomic2 θ) = showAtomic θ
  inspect σ (Address2 α) = "&" ++ show α ++ "[" ++ showCompound π ++ "]"
    where (Compound2 π) = σ Data.Map.! α

instance ShowIndent Value2 where
  showIndent ι (Atomic2 δ) = showIndent ι δ
  showIndent ι (Address2 α) = "&" ++ show α

instance ShowIndent Element2 where
  showIndent ι (Compound2 δ) = showIndent ι δ

----------------------------------------------------
-- System3: Data Passed-By-Reference & Interning  --
----------------------------------------------------

data Value3 = Address3 Address

instance Eq Value3 where
  (Address3 α) == (Address3 α') = α == α'

data Element3 = Data3 (Data Value3)

interns :: [Atomic] 
interns = [Null, Boolean True, Boolean False, Number 0, Number $ 0/0]

instance System Value3 Element3 where
  dataToValue σ δ = maybe (σ', Address3 α) ifJust μ 
    where μ = (toAtomic δ) >>= (`Data.List.elemIndex` interns) 
          ifJust ν = (σ, Address3 $ fromIntegral ν)
          α = fresh σ
          σ' = Data.Map.insert α (Data3 δ) σ
  valueToData σ (Address3 α) = δ
    where (Data3 δ) = σ Data.Map.! α
  mutate σ (Address3 α) δ
    | fromIntegral α < length interns = Left "Mutations disabled"
    | otherwise = Right $ Data.Map.insert α (Data3 δ) σ
  initialStore = Data.Map.fromAscList $ zip [0..] (map (Data3 . Atomic) interns)
  inspect σ (Address3 α) = "&" ++ show α ++ "[" ++ showData δ ++ "]"
    where (Data3 δ) = σ Data.Map.! α

instance ShowIndent Value3 where
  showIndent _ (Address3 α) = "&" ++ show α

instance ShowIndent Element3 where
  showIndent ι (Data3 δ) = showIndent ι δ

-----------------------------------
-- System4: Data Passed-By-Value --
-----------------------------------

data Value4 = Data4 (Data Value4)

instance Eq Value4 where
  (Data4 δ) == (Data4 δ') = error "data are not equalable"

data Element4 = Void

instance System Value4 Element4 where
  dataToValue σ δ = (σ, Data4 δ)
  valueToData _ (Data4 δ) = δ
  mutate _ _ _ = Left "System \"inline\" does not support mutation"
  inspect _ (Data4 δ) = showData δ

instance ShowIndent Value4 where
  showIndent ι (Data4 δ) = showIndent ι δ

instance ShowIndent Element4 where
  showIndent _ (Void) = error "Element4 should never be instantiated"

----------
-- Main --
----------

builtins :: [Builtin]
builtins = [
  -- applyBuiltin
  "apply",
  "read-line",
  "print",p
  -- applyPure
  "begin",
  "eq?",
  "inspect",
  "cons",
  "list",
  "car",
  "cdr",
  "set-car!",
  "set-cdr!",
  -- applyData
  "raise",
  "equal?",
  "any->string",
  "null?",
  "boolean?",
  "number?",
  "string?",
  "pair?",
  "procedure?",
  -- applyAtomic
  "||",
  "&&",
  "=",
  "!=",
  "<=",
  "+",
  "-",
  "*",
  "/",
  "expt",
  "sqrt",
  "number->string",
  "string->number",
  "string-length",
  "string-append",
  "substring"]

execute :: (ShowIndent v, ShowIndent e, System v e) => Control -> IO (State v e)
execute γ = let (σ, ε) = Data.List.foldl' accumulator (initialStore, Data.Map.empty) builtins
            in loop $ Ongoing γ ε σ Finish
  where loop φ@(Ongoing _ _ _ _) = step φ >>= loop -- putStrLn (showIndent 0 φ) >> 
        loop φ = return φ
        accumulator (σ, ε) β = let (σ', ξ) = dataToValue σ (Atomic $ Builtin β)
                               in (σ', Data.Map.insert β ξ ε)

top :: String -> Either Text.Parsec.ParseError Control -> IO String
top _ (Left ε) = return $ show ε
top "reference" (Right γ) = fmap printState (execute γ :: IO (State Value1 Element1))
top "mixed" (Right γ) = fmap printState (execute γ :: IO (State Value2 Element2))
top "reference-interning" (Right γ) = fmap printState (execute γ :: IO (State Value3 Element3))
top "value" (Right γ) = fmap printState (execute γ :: IO (State Value4 Element4))
top λ _ = return $ "Error >> unknown system " ++ λ

printState :: (ShowIndent x, ShowIndent y, System x y) => State x y -> String
-- printState = showIndent 0
printState φ@(Ongoing _ _ _ _) = "Error >> expected a final state but got: " ++ showIndent 0 φ
printState φ@(Failure σ κ (τ, τ')) = "Failure >> " ++ τ ++ ": " ++ τ'
printState φ@(Success σ ξ) = "Success >> " ++ (showData $ valueToData σ ξ)

main :: IO ()
main = do νs <- System.Environment.getArgs
          if length νs /= 2
          then do putStrLn "Usage: cesk (reference|value|mixed|reference-interning) programs.scm"
                  putStrLn "       cesk instrument[-literal][-variable][-binding][-condition][-abstraction][-application] program.scm"
          else if Data.List.isPrefixOf "instrument" (head νs)
               then Text.Parsec.String.parseFromFile (parser $ head νs) (head $ tail νs) >>= putStrLn . either show (showIndent 0) 
               else Text.Parsec.String.parseFromFile (parser "") (head $ tail νs) >>= top (head νs) >>= putStrLn -- >> return () -- 
