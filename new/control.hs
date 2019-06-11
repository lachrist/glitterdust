{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

-- CE*SK

import qualifid Data.Map.Strict as Map

---------------
-- Control γ --
---------------

type SymbolID = Int
type SymbolDescription = String

data Primitive = Boolean Bool
               | String String
               | Symbol SymbolID SymbolDescription
               | Number Float
               | Null
               | Undefined

type Identifier = String

type Label = String

type BuiltinName = String

type UnaryOperator = String

type BinaryOperator = String

type Block = ([Label], [Identifier], [Statement])

data ArgumentName = ArgumentLength
                  | ArgumentThis
                  | ArgumentIndex Int 

data Expression = Read Identifier
                | Write Identifier Expression Expression
                | Literal Primitive
                | Function Block
                | Conditional Expression Expression Expression
                | Sequence Expression Expression
                | Apply Expression Expression [Expression]
                | Construct Expression [Expression]
                | Eval Expression
                | Error
                | Argument ArgumentName
                | Builtin BuiltinName
                | Unary UnaryOperator Expression
                | Binary BinaryOperator Expression Expression

data Statement = Expression Expression
               | While Expression Block
               | If Expression Block Block
               | Return Expression
               | Throw Expression
               | Try Block Block Block
               | Block Block
               | Break Label
               | Continue Label
               | Switch Block
               | Case (Maybe Expression)

type Control = Either Expression Statement

-----------------
-- Environment --
-----------------

type EnvironmentAddress = Int

type ValueStackLength = Int

type ResumeStackLength = Int

data FrameKind = TryFrame | ClosureFrame | BlockFrame | SwitchFrame | WhileFrame

data Frame = TryFrame v [Label] ValueStackLength ResumeStackLength
           | BlockFrame [Label] ResumeStackLength
           | BlockFrame [Label]
           | ClosureFrame v v [v] [Label] ResumeStackLength
           | 

type Frame = (FrameKind, [Label], ValueStackLength, ResumeStackLength)

type Environment v = ([[(Frame, EnvironmentAddress)]], Map.Map EnvironmentAddress (Map.Map Identifier (Maybe v)))

insert :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
insert a1 b1 ((a2, b2) : ps) = if a1 == a2 then (a1, b1) : ps else (a2, b2) : insert a1 b1 ps
insert a  b  [] = [(a, b)]

extend :: (System v x) => Frame -> [Identifier] -> Environment v -> Environment v
extend f is (ps : pss, m) = ((f, a) : ps : pss, IntMap.insert a (zip is (repeat Nothing)) m) where a = fst (findMax m) + 1

fetch :: Identifier -> Environment v -> v
fetch i ((_, a) : ps : pss, m) = maybe (fetch i (ps : pss, m)) fromJust (lookup i ((IntMap.!) m a))

write :: Identifier -> v -> Environment v -> Environment v
write i v ((_, _, _, _, a) : fs) pss = maybe (write i v fs pss) ((flip IntMap.insert a) pss) (loop i ((IntMap.!) pss a))
  where loop [] = Nothing
        loop ((i', v') : ivs) = if i == i' then Just ((i, Just v) : ivs) else (i', v') : loop ivs

save :: Environment v -> [(Frame, Address)]
save (ps : pss, m) = ps

load :: [(Frame, Address)] -> Environment v -> Environment v
load ps (pss, m) = (ps : pss, m)

drop :: Environment v -> Environment v
drop (_ : pss, m) = (pss, m)

goto :: (Frame -> Bool) -> Environment v -> Environment v
goto p ((f, a) : ps : pss, m)
 | p f = ((f, a) : ps : pss, m)
 | otherwise = goto (ps : pss, m)

peek :: Environment v -> Frame
peek ((f, _) : _ : _, _) = f

-----------
-- Store --
-----------

type Address = Int

type Store x = Map.Map Address x

--------------------
-- Kontinuation κ --
--------------------

type Kontinuation v = ([v], [Resume])

data Resume = ResumeWrite Identifier Expression
            | ResumeSequence Expression
            | ResumeConditional Expression Expression
            | ResumeUnary UnaryOperator
            | ResumeBinary BinaryOperator (Maybe (Environment Expression))

-----------
-- State --
-----------

type State v x = (Control, Environment v, Store x, Kontinuation v)

-----------
-- Value --
-----------

type PropertyKey = Either String SymbolID 

data PropertyDescriptor v = Data v Bool Bool Bool
                          | Accessor v v Bool Bool

type Mapping v = ([(PropertyKey, PropertyDescriptor v)], Bool, v)

data Compound v = Object (Mapping v)
                | Array (Mapping v) Int
                | Closure (Mapping v) [Frame] Block
                | BBuiltin (Mapping v) BuiltinName 
                | Proxy v v

primitiveToBool :: Primitive -> Bool
primitiveToBool Undefined       = False
primitiveToBool (Boolean False) = False
primitiveToBool (Number 0)      = False
primitiveToBool (String [])     = False
primitiveToBool _               = True

class System v x | v -> x where
  primitiveToValue :: Primitive -> Store x -> (v, Store x)
  compoundToValue :: Compound v -> Store x -> (v, Store x)
  primordialToValue :: String -> Store x -> v
  valueToBool :: v -> Store x -> Bool
  valueToMaybeNumber :: v -> Store x -> Maybe Float
  valueToMaybeString :: v -> Store x -> Maybe String
  valueToMaybeCompound :: v -> Store x -> Maybe (Compound v)

type Value1 = Address
type Stored1 = Either Primitive (Compound Value1)
type State1 = State Value1 Stored1

instance System Value1 Stored1 where
  primitiveToValue p xs = (length xs, xs ++ [Left p])
  compoundToValue c xs = (length xs, xs ++ [Right c])
  valueToBool a xs = either primitiveToBool (const True) (xs!!a)

type Value2 = Either Primitive Address
type Stored2 = Compound Value2
type State2 = State Value2 Stored2

instance System Value2 Stored2 where
  primitiveToValue p xs = (Left p, xs)
  compoundToValue c xs = (Right (length xs), xs ++ [c])
  valueToBool (Left p) _ = primitiveToBool p
  valueToBool _ _ = True

jump :: Maybe l -> -> [Frame v] -> [Frame v]
jump b Nothing (FrameWhile)

filter :: Maybe l -> Frame v -> Bool
filter Nothing (FrameWhile, _, _, _) = 

isNotWhileOrSwitch :: Frame v -> Bool
isNotWhileOrSwitch (WhileFrame, _, _, _, _) = False
isNotWhileOrSwitch (SwitchFrame, _, _, _, _) = False
isNotWhileOrSwitch _ = True

isNotWhile :: Frame v -> Bool
isNotWhile (WhileFrame, _, _, _, _) = False
isNotWhileOrSwitch _ = True

isNotLabeled :: Label -> Frame v -> Bool
isNotLabeled l (_, ls, _, _, _) = notElem l ls

step :: (System v x) => State v x -> State v x
step (Left (Literal p), ε, σ, κ) = kontinue v ε σ' κ
  where (σ', v) = primitiveToValue p σ
step (Left (Closure b), ε@(fs:_, _), σ, κ) = kontinue v ε σ' κ
  where (σ', v) = compoundToValue (Function ([], True, nameToValue "Function.prototype" xs) fs b) xs
step (Left (Builtin p), ε, σ, κ) = kontinue (primordialToValue p σ) ε σ κ
step (Left (Read i), ε@(fs:_, pss), σ, κ) = kontinue (fetch i fs pss) ε σ κ
step (Left (Conditional e e' e''), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeConditional e' e'' : rs))
step (Left (Sequence e e'), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeSequence e' : rs))
step (Left (Unary o e), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeUnary o : rs))
step (Left (Binary o e e'), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeBinary o (Just e') : rs))
step (Left (Construct e es), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeConstruct 0 es : rs))
step (Left (Apply e e' es), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeApply 0 (e' : es) : rs))
step (Left (Write i e e'), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeWrite i e' : rs))
step (Right (If e b b'), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeIf b b' ss : rs))
step (Right (While e b), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeWhile e b ss : rs))
step (Right (Expression e), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeExpression ss : rs))
step (Right (Block b), ε, σ, κ) = block FrameBlock b ε σ κ
step (Right (Try b b' b''), ε, σ, (vs, rs)) = block FrameTry b ε σ (vs, ResumeTry (Just b') b'' : rs)
step (Right (Switch b), ε, σ, κ) = block FrameSwitch b ε σ κ
step (Right (Case _), ε, σ, κ@(_, ResumeBlock _)) = kontinue Nothing ε σ κ
step (Right (Case Nothing), ε, σ, (vs, ResumeSwitch ss ss')) = kontinue Nothing ε σ (vs, ResumeSwitch (dropWhile isNotCase ss) ss')
step (Right (Case (Just e)), ε, σ, κ) = (Left e, ε, σ, (vs, ResumeCase, κ))
step (Right (Return e), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeReturn : rs))
step (Right (Break ml)), (fs : fss, pss), σ, (vs, rs)) = kontinue Nothing (fs' : fss, pss) σ (vs, drop (length rs - n + 1) rs)
  where ((_, _, _, n, _) : fs') = dropWhile (maybe isNotWhileOrSwitch isNotLabeled ml) fs
step (Right (Continue ml), (fs : fss, pss), σ, (vs, rs) = kontinue Nothing (fs' : fss, pss) σ (vs, drop (length rs - n + 1) rs)
  where ((_, _, _, n, _) : fs') = dropWhile (maybe isNotWhile isNotLabeled ml) fs
step (Right (Throw e), ε, σ, (vs, rs)) = (Left e, ε, σ, (vs, ResumeThrow : rs))

-- consumers
-- * test
-- * drop
-- * return

kontinue :: (System v x) => Maybe v -> Environment v -> Store v x -> Kontinuation v -> State v x
kontinue (Just v) ε σ (vs, ResumeConditional e e' : rs) = (Left (if valueToBool v σ then e else e'), ε, σ, (vs, rs))
kontinue (Just _) ε σ (vs, ResumeSequence e : rs) = (Left e, ε, σ, (vs, rs))
kontinue (Just v) ε σ (vs, ResumeUnary o : rs) = either (\ (v', σ') -> abrupt v' ε σ' (vs, rs))
                                                        (\ (v', σ') -> kontinue (Just v') ε σ' (vs, rs))
                                                        (performUnary o v σ)
kontinue (Just v) ε σ (vs, ResumeBinary o (Just e) : rs) = (Left e, ε, σ, (v : vs, ResumeBinary o Nothing : rs))
kontinue (Just v) ε σ (v' : vs, ResumeBinary o Nothing : rs) = either (\v'', σ') -> abrupt v'' ε σ' (vs, rs)
                                                                      (\v'',  σ') -> kontinue v'' ε σ' (vs, rs)
                                                                      (performBinary o v v' σ)
kontinue (Just v) ε σ (vs, ResumeConstruct n (e:es) : rs) = (Left e, ε, σ, (v : vs, RConstruct (n + 1) es : rs))
kontinue (Just v) ε σ (vs, ResumeConstruct n [] : rs) = construct (reverse $ take n (v : vs)) ε σ ((drop n (v : vs)), rs)
kontinue (Just v) ε σ (vs, ResumeApply n (e:es) : rs) = (Left e, ε, σ, (v : vs, RApply (n + 1) es : rs))
kontinue (Just v) ε σ (vs, ResumeApply n [] : rs) = apply (reverse $ take n (v : vs)) ε σ ((drop n (v : vs)), rs)
kontinue (Just v) (fs : fss, pss) σ (vs, ResumeWrite i e : rs) = (Left e, (fs : fss, write i v fs pss), σ, (vs, rs))
kontinue (Just v) ε σ (vs, ResumeIf b1 b2 : rs) = block FrameIf (if valueToBool v σ then b1 else b2) ε σ κ
kontinue (Just v) (_ : fss, pss) σ (vs, ResumeReturn : rs) = kontinue (Just v) (fss, pss) σ (vs, rs)
kontinue (Just v) ε σ (vs, ResumeExpression : rs) = kontinue Nothing ε σ (vs, rs)
kontinue (Just v) ε σ κ@(vs, ResumeWhile _ b : rs)
  | valueToBool v σ = block FrameWhile b ε σ κ
  | otherwise = kontinue Nothing ε σ (vs, rs)
kontinue (Just v) ε σ (vs, ResumeThrow : rs) = abrupt v ε σ (vs, rs)
kontinue (Just v) ε σ (vs, ResumeCase : (ResumeSwitch ss ss' : rs))
  | valueToBool v σ = kontinue Nothing ε σ (vs, ResumeBlock ss : rs)
  | otherwise = kontinue Nothing ε σ (vs, ResumeSwitch (dropWhile isNotCase ss) ss')
kontinue Nothing ε σ (vs, ResumeSwitch (s:ss) ss') = (Right s, ε, σ, (vs, ResumeSwitch ss ss'))
kontinue Nothing ε σ (vs, ResumeSwitch [] ss') = kontinue Nothing ε σ (vs, ResumeBlock (dropWhile isNotDefaultCase ss'))
kontinue Nothing (_ : fss, pss) σ (vs, ResumeBlock [] : rs) = kontinue Nothing (fss, pss) σ (vs, rs)
kontinue Nothing ε σ (vs, ResumeBlock (s : ss) : rs) = (Right s, ε, σ, (vs, ResumeBlock ss : rs))

kontinue Nothing ε σ (v : vs, ResumeFinally [] : rs) = abrupt v ε σ (vs, rs)
kontinue Nothing ε σ (vs, ResumeFinally (s : ss) : rs) = (Right s, ε, σ, (vs, ResumeFinally ss))

abrupt :: (System v x) => v -> Environment v -> Store v x -> Kontinuation -> State
abrupt v ([], pss) (vs, rs) = 
abrupt v (((FrameClosure, _, _, _, _) : _) : fss, pss) (vs, rs) = abrupt v (fss, pss)
abrupt v (((TryFrame, _, n, n', _) : fs) : fss, pss) (vs, rs) = 
abrupt v (((CatchFrame, _, , n', _) : fs) : fss, pss) (vs, rs) = abrupt
abrupt v (((FinallyFrame, _, , _)))


apply :: (System v x) => Maybe (Compound v) -> [v] -> Environment v -> Store x -> Kontinuation -> State
apply (Just Function _ (is, ls, s:ss) fs) (fss, pss) σ κ = (Right s, FrameClosure, )
  where (a, pss') = extend 

 | 
 where c = valueToCompound v' σ

applyBuiltin :: BuiltinName -> Value1 -> [Value1] -> Environment Value1 -> 

applyBuiltin :: BuiltinName -> v -> [v] -> Environment v -> Store x -> Kontinuation -> State
applyBuiltin "ReferenceError" _ (v: _) ε σ κ = maybe (throw ) -> do
  s <- valueToMaybeString v σ
  let c = 
  let σ' = IntMap.insert
  
  where mp = valueToMaybePrimitive v σ

applyBuiltin "Reflect.getPrototypeOf" _ (v : vs) = 
applyBuiltin "Reflect.get" _ (v : v' : _) = do
  c <- value
applyBuiltin "Reflect.set" _ (v : v' : v'' : _) = 
applyBuiltin "Reflect.apply" _ (v : v' : v'' : _) = 
applyBuiltin "Reflect.construct" _ (v : v' : v'' : _) =
applyBuiltin "Reflect.defineProperty" _ (v : v')

applyReflectGet :: Maybe (Compound v) -> Maybe (String) -> v -> Environment v -> Store x -> Kontinuation -> State
applyReflectGet (Just (Object )) (Just s) v ε σ κ = 

--kontinue Nothing  σ           (vs,      εs, RWhile ε e b            : rs) = (While e b, ε, σ, (vs, εs, rs))


-- step1 (Left (Read n), ε, σ@(fs, _), κ) = loop ε
--   where loop (a:as) = maybe (loop as) (kontinue1 (ε, σ, κ)) (lookup (fs!!a) n)
--         loop _      = error "this should never happen"
-- step1 (Left (Closure ss), fs, is, (vs, fss, rs)) = kontinue1 (vs, fs:fss, rs) (is ++ [Mutable $ Function (True, [], builtin "Function.prototype") fs ss]) (Right $ length is) 
-- step1 (Left (LiteralBoolean b), ε, σ, κ) = kontinue1 (ε, σ, κ) (Left p)
-- 
-- step1 (Left (Write n e), ε, σ, (vs, cs, ks)) = step1 (Left e, ε, σ, (vs, cs, RWrite n : ks))
-- step1 (Left (Conditional e1 e2 e3), ε, σ, (vs, cs, ks)) = step1 (Left e1, ε, σ, (vs, cs, RConditional e2 e3 : ks))
-- step1 (Left (Sequence e1 es e2), ε, σ, (vs, cs, rs)) = step1 (Left e1, ε, σ, RSequence es e2 : (vs, cs, ks))
-- 
-- step1 (Right (Block s ss), ε, (fs, is), (vs, cs, rs)) = step1 (Right s, ()ε)
-- step1 (Right (Expression e), ε, σ, (vs, cs, rs)) = step1 
-- step1 (Right (Apply e es), fs, is, (vs, cs, rs)) = step1 (Left e1, fs, is, (vs, rs, RApply es))
-- 
-- branch1 :: Value -> Boolean
-- branch1 (Left Null = False
-- branch1 (Left (Number 0)) = False
-- branch1 (Left (String "")) = False
-- branch1 (Left (Boolean False)) = False
-- branch1 _ = True
-- 
-- kontinue1 :: Kontinuation Value1 -> Store Value1 Item1 -> Value1 -> State 1
-- kontinue1 (vs, cs, rs) s v = (v:vs, cs, rs) v
-- 
-- -- Isolate parts of the interpreter which is different from the two interpreters!!!!!!!
-- perform :: Resume -> ([Value1], [Environment], [Frame], [Item]) -> ()
-- 
-- perform1 :: [Resume] -> ([Value1], [Environment], [Frame], [Item]) -> State1
-- perform1 (v:vs, e:es, fs, is) = (Left expr1, e, (fs, is), (vs, es, rs))
-- 
-- 
-- 
-- kontinue1 :: Kontinuation v -> Store  -> State v s
-- kontinue1 (v:vs, ε:cs, RConditional e1 e2 : rs) σ = (Left if branch1 v then e1 else e2, ε, σ, (vs, cs, rs))
-- kontinue1 (v:vs, cs, RSequence es e : rs) σ = (Left $ head es ++ [e], ε, σ, (vs, cs, null es ? rs : RSequence (tail es) e))
-- kontinue1 (vs, ε:εs, RApply n (e:es) : rs) σ v = (Left $ e, ε, σ, (v:vs, εs, RApply n es : rs)
-- kontinue1 (vs, ε:εs, RApply n [] : rs) σ v = 
-- 
-- 
-- 
-- 
--   (Left $ e, ε, σ, (v:vs, εs, RApply es : rs)
-- 
-- 
-- kontinue1 :: v -> (e, s, (vstack, cstack, FConditional expression1 expression2)) = 
-- 
-- 
-- 00:41.66
