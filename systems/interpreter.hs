
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as Map

-------------
-- Control --
-------------

type Identifier = String

data Primitive = Null
               | Bool Bool
               | Float Float
               | String String
               deriving (Eq, Show)

data Control = Literal Primitive
             | Identifier Identifier
             | If Control Control Control
             | Let Identifier Control Control
             | Abstraction [Identifier] Control
             | Application Control [Control]
             deriving (Eq, Show)

-----------------
-- Environment --
-----------------

type Environment v = Map.Map Identifier v

-----------
-- Store --
-----------

type Address = Int

type Store x = Map.Map Address x

store :: x -> Store x -> (Address, Store x)
store χ σ = (Address1 α, Map.insert α χ σ)
  where α = maybe 0 ((+1) . fst) (Map.lookupMax σ)

store' :: x -> Store x -> (Address, Store x)
store' χ σ = maybe (store χ σ) (,σ) (Map.foldlWithKey' acc Nothing σ)
  where acc Nothing α χ' = if χ == χ' then Just α else Nothing
        acc (Just α) _ _ = Just α

------------------
-- Kontinuation --
------------------

data Kontinuation v = Write (Environment v) Identifier Control (Kontinuation v)
                    | Apply (Environment v) [Control] [v] (Kontinuation v)
                    | Branch (Environment v) Control Control (Kontinuation v)
                    | Final

-----------
-- State --
-----------

type Ongoing v x = (Control, Environment v, Store x, Kontinuation v)

type Final v x = Either String (v, Store x)

type State v x = Either (Final v x) (Ongoing v x)

------------
-- System --
------------

builtins :: [Builtin]
builtins = ["identical?", "equal?", "cons", "car", "cdr", "set-car!", "set-cdr!", "+", "-", "*", "/", "<"]

type Builtin = String

type Closure v = (Environment v, [Identifier], Control)

type Cons v = (v, v)

class System v x where
  primitiveToValue :: Primitive -> Store x -> (v, Store x)
  closureToValue :: Closure v -> Store x -> (v, Store x)
  valueToBool :: v -> Store x -> Bool
  valueToMaybeApplicable :: v -> Store x -> Maybe (Either (Closure v) Builtin)
  applyBuiltin :: Builtin -> [v] -> Store x -> Kontinuation v -> State v x

-----------------
-- Interpreter --
-----------------

run :: (System v x) => State v x -> Final v x
run (Right ο) = run $ step ο
run (Left φ) = φ

step :: (System v x) => Ongoing v x -> State v x
step (Primitive ρ, ε, σ, κ) = kontinue κ (primitiveToValue ρ σ)
step (Abstraction ιs γ, ε, σ, κ) = kontinue κ (closureToValue (ε, ιs, γ) σ)
step (Identifier ι, ε, σ, κ) = maybe (Left $ Left "Undeclared identifier")
                                     (kontinue κ . (, σ))
                                     (Map.lookup ι ε)
step (Let ι γ γ', ε, σ, κ) = Right (γ, ε, σ, Write ε ι γ' κ)
step (Application γ γs, ε, σ, κ) = Right (γ, ε, σ, Apply ε γs [] κ)
step (If γ γ' γ'', ε, σ, κ) = Right (γ, ε, σ, Branch ε γ' γ'' κ)

kontinue :: (System v x) => Kontinuation v -> (v, Store x) -> State v x
kontinue Final (ν, σ) = Left (Right (ν, σ))
kontinue (Branch ε γ γ' κ) (ν, σ) = Right (if valueToBool ν σ then γ else γ', ε, σ, κ)
kontinue (Write ε ι γ κ) (ν, σ) = Right (γ, Map.insert ι ν ε, σ, κ)
kontinue (Apply ε (γ:γs) νs κ) (ν, σ) = Right (γ, ε, σ, Apply ε γs (ν:νs) κ)
kontinue (Apply ε [] νs κ) (ν, σ) = apply (valueToMaybeApplicable (last $ ν:νs) σ) (tail $ reverse $ ν:νs) σ κ

apply :: (System v x) => Maybe (Either (Closure v) Builtin) -> [v] -> Store x -> Kontinuation v -> State v x
apply (Just (Left (ε, ιs, γ))) νs σ κ = Right (γ, Map.union (Map.fromList (zip ιs νs)) ε, σ, κ)
apply (Just (Right β)) νs σ κ = applyBuiltin β νs σ κ
apply _ _ _ _ = Left $ Left "Only closures or builtins can be applied"

--------------
-- System 1 --
--------------

data Value1 = Address1 Address deriving (Eq, Show)

data Element1 = Primitive1 Primitive
              | Builtin1 Builtin
              | Closure1 (Closure Value1)
              | Cons1 (Cons Value1)
              deriving (Eq, Show)

applyBuiltinHelper1 :: Builtin -> [Element1] -> Store Element1 -> Kontinuation Value1 -> State Value1 Element1
applyBuiltinHelper1 "equal?" (ν:ν':_) σ κ = kontinue κ (store1 (Primitive1 $ Bool $ ν == ν') σ)
applyBuiltinHelper1 "car" (Cons1 (ν, _) : _) σ κ = kontinue κ (ν, σ)
applyBuiltinHelper1 "cdr" (Cons1 (_, ν) : _) σ κ = kontinue κ (ν, σ)
applyBuiltinHelper1 "+" (Primitive1 (Float ζ) : Primitive1 (Float ζ') : _) σ κ = kontinue κ (store1 (Primitive1 $ Float $ ζ + ζ') σ)
applyBuiltinHelper1 "-" (Primitive1 (Float ζ) : Primitive1 (Float ζ') : _) σ κ = kontinue κ (store1 (Primitive1 $ Float $ ζ - ζ') σ)
applyBuiltinHelper1 "*" (Primitive1 (Float ζ) : Primitive1 (Float ζ') : _) σ κ = kontinue κ (store1 (Primitive1 $ Float $ ζ * ζ') σ)
applyBuiltinHelper1 "/" (Primitive1 (Float ζ) : Primitive1 (Float ζ') : _) σ κ = kontinue κ (store1 (Primitive1 $ Float $ ζ / ζ') σ)
applyBuiltinHelper1 "<" (Primitive1 (Float ζ) : Primitive1 (Float ζ') : _) σ κ = kontinue κ (store1 (Primitive1 $ Bool $ ζ < ζ') σ)
applyBuiltinHelper1 _ _ _ _ = Left $ Left "Builtin application failure"

instance System Value1 Element1 where
  primitiveToValue τ σ = store1 (Primitive1 τ) σ
  closureToValue λ σ = store1 (Closure1 λ) σ
  valueToBool (Address1 α) σ = inner $ σ Map.! α
    where inner (Primitive1 (Bool False)) = False
          inner _ = True
  valueToMaybeApplicable (Address1 α) σ = inner $ σ Map.! α
    where inner (Closure1 λ) = Just $ Left λ
          inner (Builtin1 β) = Just $ Right β
          inner _ = Nothing
  applyBuiltin "identical?" (ν:ν':_) σ κ = kontinue κ (store1 (Primitive1 $ Bool $ ν == ν') σ)
  applyBuiltin "cons" (ν:ν':_) σ κ = kontinue κ (store1 (Cons1 $ (ν, ν')) σ)
  applyBuiltin "set-car!" (Address1 α : ν : _) σ κ = inner $ σ Map.! α
    where inner (Cons1 (_, ν')) = kontinue κ (ν, Map.insert α (Cons1 (ν, ν')) σ)
          inner _ = Left $ Left "Attempt to apply set-car! on non-cons"
  applyBuiltin "set-cdr!" (Address1 α : ν : _) σ κ = inner $ σ Map.! α
    where inner (Cons1 (ν', _)) = kontinue κ (ν, Map.insert α (Cons1 (ν', ν)) σ)
          inner _ = Left $ Left "Attempt to apply set-cdr! on non-cons"
  applyBuiltin β νs σ κ = applyBuiltinHelper1 β (map (\(Address1 α) -> σ Map.! α) νs) σ κ

eval1 :: Control -> Final Value1 Element1
eval1 γ = run $ Right (γ, Map.fromList (zip builtins (map Address1 [0..])), Map.fromList (zip [0..] (map Builtin1 builtins)), Final)

--------------
-- System 2 --
--------------

data Value2 = Address2 Address
            | Primitive2 Primitive
            | Builtin2 Builtin
            | Closure2 (Closure Value2)
            deriving (Eq, Show)

data Element2 = Cons2 (Cons Value2) deriving (Show)

load2 :: Address -> Store Element2 -> (Value2, Value2)
load2 α σ = (\ (Cons2 (ν, ν')) -> (ν, ν')) (σ Map.! α)

instance System Value2 Element2 where
  primitiveToValue ρ σ = (Primitive2 ρ, σ)
  closureToValue λ σ = (Closure2 λ, σ)
  valueToBool (Primitive2 (Bool False)) _ = False
  valueToBool _ _ = True
  valueToMaybeApplicable (Closure2 λ) _ = Just $ Left λ
  valueToMaybeApplicable (Builtin2 β) _ = Just $ Right β
  applyBuiltin "identical?" (ν:ν':_) σ κ = kontinue κ (Primitive2 $ Bool $ ν == ν', σ)
  applyBuiltin "cons" (ν:ν':_) σ κ = kontinue κ (store (Cons2 (ν, ν')) σ)  (Address2 α, Map.insert α )
    where α = maybe 0 ((+1) . fst) (Map.lookupMax σ)
  applyBuiltin "car" (Address2 α : _) σ κ = kontinue κ (fst $ load2 α σ, σ)
  applyBuiltin "cdr" (Address2 α : _) σ κ = kontinue κ (snd $ load2 α σ, σ)
  applyBuiltin "set-car!" (Address2 α : ν : _) σ κ = kontinue κ (ν, Map.insert α (Cons2 (ν, snd $ load2 α σ)) σ)
  applyBuiltin "set-cdr!" (Address2 α : ν : _) σ κ = kontinue κ (ν, Map.insert α (Cons2 (fst $ load2 α σ, ν)) σ)
  applyBuiltin "+" (Primitive2 (Float ζ) : Primitive2 (Float ζ') : _) σ κ = kontinue κ (Primitive2 $ Float $ ζ + ζ', σ)
  applyBuiltin "-" (Primitive2 (Float ζ) : Primitive2 (Float ζ') : _) σ κ = kontinue κ (Primitive2 $ Float $ ζ - ζ', σ)
  applyBuiltin "*" (Primitive2 (Float ζ) : Primitive2 (Float ζ') : _) σ κ = kontinue κ (Primitive2 $ Float $ ζ * ζ', σ)
  applyBuiltin "/" (Primitive2 (Float ζ) : Primitive2 (Float ζ') : _) σ κ = kontinue κ (Primitive2 $ Float $ ζ / ζ', σ)
  applyBuiltin "<" (Primitive2 (Float ζ) : Primitive2 (Float ζ') : _) σ κ = kontinue κ (Primitive2 $ Bool $ ζ < ζ', σ)
  applyBuiltin _ _ _ _ = Left $ Left "Application failure"

eval2 :: Control -> Final Value2 Element2
eval2 γ = run $ Right (γ, Map.fromList (zip builtins (map Builtin2 builtins)), Map.empty, Final)

--------------
-- System 3 --
--------------

data Value3 = Address3 Address deriving (Eq, Show)

data Element3 = Primitive3 Primitive
              | Builtin3 Builtin
              | Closure3 (Closure Value3)
              | Cons3 (Cons Value3)
              deriving (Eq, Show)

applyBuiltinHelper3 :: Builtin -> [Element3] -> Store Element3 -> Kontinuation Value3 -> State Value3 Element3
applyBuiltinHelper3 "equal?" (ν:ν':_) σ κ = kontinue κ (store3 (Primitive3 $ Bool $ ν == ν') σ)
applyBuiltinHelper3 "car" (Cons3 (ν, _) : _) σ κ = kontinue κ (ν, σ)
applyBuiltinHelper3 "cdr" (Cons3 (_, ν) : _) σ κ = kontinue κ (ν, σ)
applyBuiltinHelper3 "+" (Primitive3 (Float ζ) : Primitive3 (Float ζ') : _) σ κ = kontinue κ (store3 (Primitive3 $ Float $ ζ + ζ') σ)
applyBuiltinHelper3 "-" (Primitive3 (Float ζ) : Primitive3 (Float ζ') : _) σ κ = kontinue κ (store3 (Primitive3 $ Float $ ζ - ζ') σ)
applyBuiltinHelper3 "*" (Primitive3 (Float ζ) : Primitive3 (Float ζ') : _) σ κ = kontinue κ (store3 (Primitive3 $ Float $ ζ * ζ') σ)
applyBuiltinHelper3 "/" (Primitive3 (Float ζ) : Primitive3 (Float ζ') : _) σ κ = kontinue κ (store3 (Primitive3 $ Float $ ζ / ζ') σ)
applyBuiltinHelper3 "<" (Primitive3 (Float ζ) : Primitive3 (Float ζ') : _) σ κ = kontinue κ (store3 (Primitive3 $ Bool $ ζ < ζ') σ)
applyBuiltinHelper3 _ _ _ _ = Left $ Left "Builtin application failure"

instance System Value3 Element3 where
  primtiveToValue τ σ = store3 (Primitive3 τ) σ
  closureToValue λ σ = store3 (Closure3 λ) σ
  valueToBool (Address3 α) σ = inner $ σ Map.! α
    where inner (Primitive3 (Bool False)) = False
          inner _ = True
  valueToMaybeApplicable (Address3 α) σ = inner $ σ Map.! α
    where inner (Closure3 λ) = Just $ Left λ
          inner (Builtin3 β) = Just $ Right β
          inner _ = Nothing
  applyBuiltin "identical?" (ν:ν':_) σ κ = kontinue κ (store3 (Primitive3 $ Bool $ ν == ν') σ)
  applyBuiltin "cons" (ν:ν':_) σ κ = kontinue κ (store3 (Cons3 $ (ν, ν')) σ)
  applyBuiltin "set-car!" (Address3 α : ν : _) σ κ = inner $ σ Map.! α
    where inner (Cons3 (_, ν')) = kontinue κ (ν, Map.insert α (Cons3 (ν, ν')) σ)
          inner _ = Left $ Left "Attempt to apply set-car! on non-cons"
  applyBuiltin "set-cdr!" (Address3 α : ν : _) σ κ = inner $ σ Map.! α
    where inner (Cons3 (ν', _)) = kontinue κ (ν, Map.insert α (Cons3 (ν', ν)) σ)
          inner _ = Left $ Left "Attempt to apply set-cdr! on non-cons"
  applyBuiltin β νs σ κ = applyBuiltinHelper3 β (map (\(Address3 α) -> σ Map.! α) νs) σ κ

eval3 :: Control -> Final Value3 Element3
eval3 γ = run $ Right (γ, Map.fromList (zip builtins (map Address3 [0..])), Map.fromList (zip [0..] (map Builtin3 builtins)), Final)

----------
-- Test --
----------

fac6 :: Control
fac6 = (Let "fac-cons"
  (Application
    (Identifier "cons")
    [
      (Literal $ Null),
      (Literal $ Null)])
  (
    Let "fac"
    (Abstraction ["x"]
      (If
        (Application
          (Identifier "<")
          [
            (Identifier "x"),
            (Literal $ Float 1)])
        (Literal $ Float 1)
        (Application
          (Identifier "*")
          [
            (Identifier "x"),
            (Application
              (Application
                (Identifier "car")
                [
                  (Identifier "fac-cons")])
              [
                (Application
                  (Identifier "-")
                  [
                    (Identifier "x"),
                    (Literal $ Float 1)])])])))
    (Let "_"
      (Application
        (Identifier "set-car!")
        [
          (Identifier "fac-cons"),
          (Identifier "fac")])
      (Application
        (Identifier "fac")
        [
          (Literal $ Float 6)]))))

yo1 :: Final Value1 Element1
yo1 = eval1 fac6

yo2 :: Final Value2 Element2
yo2 = eval2 fac6

yo3 :: Final Value3 Element3
yo3 = eval3 fac6
