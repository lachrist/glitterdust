

module Interpreter(run) where

import qualified Control.Monad.State
import qualified Data.List
import qualified Expression
import qualified Environment
import qualified Data.List
import qualified Control.Monad
import qualified Primitive
import qualified Store
import qualified Compound
import qualified Kontinuation
import qualified Parser


type EAddress = Store.Address (Environment.Environment Value)
type CAddress = Store.Address (Compound.Compound Value)
type KAddress = Store.Address (Kontinuation.Kontinuation Value)


type EStore = Store.Store (Environment.Environment Value)
type CStore = Store.Store (Compound.Compound Value)
type KStore = Store.Store (Kontinuation.Kontinuation Value)


type Loaded = Either (Compound.Compound Value) (Kontinuation.Kontinuation Value)


data Value = EPointer EAddress
           | CPointer CAddress
           | KPointer KAddress
           | Inlined Primitive.Primitive
           deriving (Eq, Show)


data State = Failure String (EStore, CStore, KStore)
           | Success Value (EStore, CStore, KStore)
           | Ongoing Expression.Expression
                     EAddress
                     (EStore, CStore, KStore)
                     KAddress
           deriving (Eq, Show)


initialize :: Expression.Expression -> Bool -> State
initialize expr stored = let expr' = if stored
                                     then fst $ Control.Monad.State.runState (Expression.visit storify expr) 0
                                     else expr
                             (cStore, vals) = foldr (\str (cStore, cAddrs) -> (Store.add (Compound.Builtin str) cStore, CPointer (Store.nxt cStore) : cAddrs)) (Store.new, []) (impures ++ map ((:) (if stored then '&' else '$')) pures)
                             eAddr1 = Store.nxt Store.new
                             eStore1 = Store.add Environment.Root Store.new
                             eAddr2 = Store.nxt eStore1
                             eStore2 = Store.add (Environment.ext (map Expression.V (impures ++ pures)) vals eAddr1) eStore1 
                         in Ongoing expr' eAddr2 (eStore2, cStore, Store.add Kontinuation.Halt Store.new) (Store.nxt Store.new)
  where impures = ["call/cc", "cons", "vcons", "car", "cdr", "set-car!", "set-cdr!"]
        pures = ["raise", "address", "eq?", "typeof"]
        storify _ (Expression.Constant prim) = Expression.StoredConstant prim
        storify _ expr = expr


run :: Expression.Expression -> Bool -> String
run expr stored = loop $ initialize expr stored
  where loop (Failure str _)     = str
        loop (Success val store) = show $ load val store
        loop state               = loop $ step state


step :: State -> State
step (Failure msg store) = Failure msg store
step (Ongoing (Expression.Set var expr)             eAddr (eStore, cStore, kStore) kAddr) = Ongoing expr  eAddr (eStore, cStore, Store.add (Kontinuation.Set var eAddr kAddr) kStore)        (Store.nxt kStore)
step (Ongoing (Expression.Def var expr)             eAddr (eStore, cStore, kStore) kAddr) = Ongoing expr  eAddr (eStore, cStore, Store.add (Kontinuation.Def var eAddr kAddr) kStore)        (Store.nxt kStore)
step (Ongoing (Expression.Branch expr1 expr2 expr3) eAddr (eStore, cStore, kStore) kAddr) = Ongoing expr1 eAddr (eStore, cStore, Store.add (Kontinuation.If expr2 expr3 eAddr kAddr) kStore) (Store.nxt kStore)
step (Ongoing (Expression.Call expr exprs)          eAddr (eStore, cStore, kStore) kAddr) = Ongoing expr  eAddr (eStore, cStore, Store.add (Kontinuation.Call exprs [] eAddr kAddr) kStore)  (Store.nxt kStore)
step (Ongoing (Expression.Eval expr)                eAddr (eStore, cStore, kStore) kAddr) = Ongoing expr  eAddr (eStore, cStore, Store.add (Kontinuation.Eval eAddr kAddr) kStore)           (Store.nxt kStore)       
step (Ongoing (Expression.Lambda vars expr)         eAddr (eStore, cStore, kStore) kAddr) = applyKont (Store.get kAddr kStore) (CPointer $ Store.nxt cStore) (eStore, Store.add (Compound.Closure vars expr eAddr) cStore, kStore)
step (Ongoing (Expression.Constant prim)            eAddr (eStore, cStore, kStore) kAddr) = applyKont (Store.get kAddr kStore) (Inlined prim)                (eStore, cStore, kStore)
step (Ongoing (Expression.StoredConstant prim)      eAddr (eStore, cStore, kStore) kAddr) = applyKont (Store.get kAddr kStore) (CPointer $ Store.nxt cStore) (eStore, Store.add (Compound.Stored prim) cStore, kStore)
step (Ongoing (Expression.Get var)                  eAddr (eStore, cStore, kStore) kAddr) = maybe (Failure ("undefined variable " ++ show var) (eStore, cStore, kStore))
                                                                                                  (\val -> applyKont (Store.get kAddr kStore) val (eStore, cStore, kStore))
                                                                                                  (Environment.get var eAddr eStore)


load :: Value -> (EStore, CStore, KStore) -> Loaded
load (CPointer cAddr) (_, cStore, _     ) = Left $ Store.get cAddr cStore
load (KPointer kAddr) (_, _     , kStore) = Right $ Store.get kAddr kStore
load (Inlined prim)   _                   = Left $ Compound.Stored prim


applyKont :: Kontinuation.Kontinuation Value -> Value -> (EStore, CStore, KStore) -> State
applyKont (Kontinuation.Branch expr1 expr2 eAddr kAddr) val store@(_, cStore, kStore) = Ongoing (next $ load val store) eAddr store kAddr
  where next (Left (Compound.Stored prim)) = maybe expr2 (\b -> if b then expr1 else expr2) (Primitive.unwrap prim)
        next _                             = expr1
applyKont (Kontinuation.Set var eAddr kAddr) val store@(eStore,cStore,kStore) = maybe (Failure ("undefined variable " ++ show var) store)
                                                                                      (\eStore' -> applyKont (Store.get kAddr kStore) val (eStore', cStore, kStore))
                                                                                      (Environment.set var val eAddr eStore)
applyKont (Kontinuation.Def var eAddr kAddr) val (eStore,cStore,kStore) = applyKont (Store.get kAddr kStore) val (Environment.def var val eAddr eStore, cStore, kStore)
applyKont (Kontinuation.Eval eAddr kAddr) val store@(_, cStore, kStore) = case load val store
                                                                          of (Left (Compound.Stored prim)) -> maybe (Failure ("cannot evaluate primitive " ++ show prim) store)
                                                                                                                    (\str -> case Parser.apply Expression.parse str
                                                                                                                             of [(expr, "")] -> Ongoing expr eAddr store kAddr
                                                                                                                                _            -> Failure ("cannot parse " ++ str) store)
                                                                                                                    (Primitive.unwrap prim)
                                                                             loaded                        -> Failure ("cannot evaluate " ++ show loaded) store
applyKont (Kontinuation.Call (expr:exprs) vals eAddr kAddr) val       (eStore, cStore, kStore) = Ongoing expr eAddr (eStore, cStore, Store.add (Kontinuation.Call exprs (vals++[val]) eAddr kAddr) kStore) (Store.nxt kStore)
applyKont (Kontinuation.Call _            vals _     kAddr) val store@(_     , cStore, kStore) = apply (load (head $ vals ++ [val]) store) (tail $ vals ++ [val]) store kAddr
applyKont Kontinuation.Halt val store = Success val store

apply :: Loaded -> [Value] -> (EStore, CStore, KStore) -> KAddress -> State
apply (Right kont) [val] store kAddr = applyKont kont val store
apply (Right _)    vals  store _     = Failure ("kontinuations expect one argument, got " ++ show (length vals)) store
apply (Left (Compound.Closure vars expr eAddr)) vals store@(eStore, cStore, kStore) kAddr = if length vars == length vals
                                                                                            then Ongoing expr (Store.nxt eStore) (Store.add (Environment.ext vars vals eAddr) eStore, cStore, kStore) kAddr
                                                                                            else Failure ("closure expected " ++ (show $ length vars) ++ " arguments, got " ++ (show $ length vals)) store 
apply (Left (Compound.Builtin "begin")) vals@(_:_) store@(_, _, kStore) kAddr = applyKont (Store.get kAddr kStore) (last vals) store
apply (Left (Compound.Builtin "call/cc")) [val] store@(eStore, cStore, kStore) kAddr = apply (load val store) [KPointer kAddr] store kAddr
apply (Left (Compound.Builtin "cons")) [val1, val2] (eStore, cStore, kStore) kAddr = applyKont (Store.get kAddr kStore) (CPointer $ Store.nxt cStore) (eStore, Store.add (Compound.Cons val1 val2) cStore, kStore)
apply (Left (Compound.Builtin "vcons")) [val1, val2, val3, val4] (eStore, cStore, kStore) kAddr = applyKont (Store.get kAddr kStore) (CPointer $ Store.nxt cStore) (eStore, Store.add (Compound.VCons val1 val2 val3 val4) cStore, kStore)
apply (Left (Compound.Builtin str)) [CPointer cAddr] store@(_, cStore, kStore) kAddr | str == "car" || str == "cdr" = case Store.get cAddr cStore
                                                                                                                      of (Compound.Cons val1 val2)      -> applyKont (Store.get kAddr kStore) (if str == "car" then val1 else val2) store
                                                                                                                         (Compound.VCons val1 val2 _ _) -> apply (load (if str == "car" then val1 else val2) store) [] store kAddr
                                                                                                                         comp                            -> Failure ("type error " ++ show comp ++ " is not a cons") store
apply (Left (Compound.Builtin str)) [CPointer cAddr, val] store@(eStore, cStore, kStore) kAddr | str == "set-car!" || str == "set-cdr!" = case Store.get cAddr cStore
                                                                                                                                          of (Compound.Cons val1 val2)      -> let comp = if str == "set-car!" then Compound.Cons val val2 else Compound.Cons val1 val
                                                                                                                                                                               in applyKont (Store.get kAddr kStore) val (eStore, Store.set cAddr comp cStore, kStore)
                                                                                                                                             (Compound.VCons _ _ val1 val2) -> apply (load (if str == "set-car!" then val1 else val2) store) [val] store kAddr
                                                                                                                                             val                            ->  Failure ("type error " ++ show val ++ " is not a cons") store
apply (Left (Compound.Builtin (c:str))) vals store@(eStore, cStore, kStore) kAddr | c == '&' || c == '$' = either (flip Failure $ store)
                                                                                                                  (\prim -> let val     = if c == '&' then CPointer $ Store.nxt cStore else Inlined prim
                                                                                                                                cStore' = if c == '&' then Store.add (Compound.Stored prim) cStore else cStore
                                                                                                                            in applyKont (Store.get kAddr kStore) val (eStore, cStore', kStore))
                                                                                                                  (applyPure str vals (map (flip load $ store) vals))
apply (Left comp) vals  store _ =  Failure ("cannot apply " ++ show comp ++ " on " ++ show vals) store


applyPure :: String -> [Value] -> [Loaded] -> Either String Primitive.Primitive
applyPure "null" [] [] = Right $ Primitive.wrap ()
applyPure "closure-parameters" _ [Left (Compound.Closure vs _ _)] = Right $ Primitive.wrap $ show vs
applyPure "address" [CPointer cAddr] _                             = Right $ Primitive.wrap $ "&c" ++ show cAddr
applyPure "address" [KPointer kAddr] _                             = Right $ Primitive.wrap $ "&k" ++ show kAddr
applyPure "address" _                _                             = Right $ Primitive.wrap $ "&i"
applyPure "eq?" _                                  [Left (Compound.Stored prim1), Left (Compound.Stored prim2)] = Right $ Primitive.wrap $ prim1 == prim2
applyPure "eq?" [CPointer cAddr1, CPointer cAddr2] _                                                            = Right $ Primitive.wrap $ cAddr1 == cAddr2
applyPure "eq?" [KPointer kAddr1, KPointer kAddr2] _                                                            = Right $ Primitive.wrap $ kAddr1 == kAddr2
applyPure "eq?" _                                  _                                                            = Right $ Primitive.wrap $ False
applyPure "typeof" _ [Left (Compound.Closure _ _ _)]          = Right $ Primitive.wrap "closure"
applyPure "typeof" _ [Left (Compound.Builtin _)]              = Right $ Primitive.wrap "builtin"
applyPure "typeof" _ [Left (Compound.Cons _ _)]               = Right $ Primitive.wrap "cons"
applyPure "typeof" _ [Left (Compound.VCons _ _ _ _)]          = Right $ Primitive.wrap "cons"
applyPure "typeof" _ [Right _ ]                               = Right $ Primitive.wrap "continuation"
applyPure name _ loadeds = Control.Monad.mapM extract loadeds >>= Primitive.apply name
  where extract (Left (Compound.Stored prim)) = Right prim
        extract loaded = Left $ show loaded ++ " is not a primitive"

