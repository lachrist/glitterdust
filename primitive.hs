{-# LANGUAGE FlexibleInstances #-}

module Primitive (Primitive, names, apply, Wrappable(wrap,unwrap)) where

import qualified Control.Monad
import qualified Data.Foldable

data Primitive = B Bool
               | F Float
               | S String
               | N
               deriving (Eq)

instance Show Primitive where
  show N = "#n"
  show (B True) = "#t"
  show (B False) = "#f"
  show (S s) = show s
  show (F f) = show f

instance Read Primitive where
  readsPrec _ ('#':'n':s) = [(N, s)]
  readsPrec _ ('#':'t':s) = [(B True, s)]
  readsPrec _ ('#':'f':s) = [(B False, s)]
  readsPrec i str = map (\(x, s) -> (F x, s)) (readsPrec i str) ++ map (\(x, s) -> (S x, s)) (readsPrec i str)

class Wrappable () where
  wrap () = Primitive N
  unwrap N = Just ()
  unwrap _ = Nothing

class Wrappable a where
  wrap :: a -> Primitive
  unwrap :: Primitive -> Maybe a

instance Wrappable Bool where
  wrap = B
  unwrap (B b) = Just b
  unwrap _     = Nothing

instance Wrappable Float where
  wrap = F
  unwrap (F f) = Just f
  unwrap _     = Nothing

instance Wrappable String where
  wrap = S
  unwrap (S s) = Just s
  unwrap _     = Nothing

names :: [String]
names = ["not", "and", "or", "+", "-", "*", "/", "<", "<=", "==", "number2string", "string2number", "string-append", "string-length", "string-ref", "string-substring"]

apply :: String -> [Primitive] -> Either String Primitive
-- raise
apply "raise" [(S s)] = Left s
-- typeof
apply "typeof" [F _] = Right $ S "number"
apply "typeof" [S _] = Right $ S "string"
apply "typeof" [B _] = Right $ S "boolean"
apply "typeof" [N]   = Right $ S "null"
-- Boolean --
apply "!"  [B False         ] = Right $ B True
apply "!"  [_               ] = Right $ B False
apply "&&" [B False, _      ] = Right $ B False
apply "&&" [_      , B False] = Right $ B False
apply "&&" [_      , _      ] = Right $ B True
apply "||" [B False, B False] = Right $ B False
apply "||" [_      , _      ] = Right $ B True
-- Number --
apply "+"  [F num1, F num2] = Right $ F $ num1 + num2
apply "-"  [F num1, F num2] = Right $ F $ num1 - num2
apply "*"  [F num1, F num2] = Right $ F $ num1 * num2
apply "/"  [F num1, F num2] = Right $ F $ num1 / num2
apply "<"  [F num1, F num2] = Right $ B $ num1 < num2
apply "<=" [F num1, F num2] = Right $ B $ num1 <= num2
apply "==" [F num1, F num2] = Right $ B $ num1 == num2
-- String --
apply "number->string"    [F flt]                 = Right $ S $ show flt
apply "string->number"    [S str]                 = case readsPrec 0 str
                                                    of [(f, "")] -> Right $ F f
                                                       _         -> Left $ "cannot parse " ++ str ++ " as a number"
apply "string-append"    [S str1, S str2       ] = Right $ S $ str1 ++ str2
apply "string-length"    [S str                ] = Right $ F $ fromIntegral $ length str
apply "string-ref"       [S str,  F flt        ] = loop (truncate flt) str
  where loop _ []     = Left $ "index out of bound"
        loop 0 (x:_)  = Right $ S $ x:[]
        loop n (_:xs) = loop (n-1) xs
apply "string-substring" [F flt1, F flt2, S str] = Right $ S $ substring (truncate flt1) (truncate flt2) str
  where substring begin end = (take (end - begin)) . (drop begin)
-- Type Error --
apply str prims = Left $ "cannot apply " ++ str ++ " on " ++ show prims
