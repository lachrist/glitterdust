module Parser (Parser, apply, fromRead, sat, char, string, blank, keyword) where

import qualified Control.Monad
import qualified Control.Applicative

newtype Parser a = P (String -> [(a,String)])

apply :: Parser a -> String -> [(a,String)]
apply (P f) s = f s

instance Functor Parser where
  fmap f p = P (\s -> map (\(x,s) -> (f x, s)) $ apply p s)

instance Applicative Parser where
  pure x = P (\s -> [(x,s)])
  p1 <*> p2 = P (\s -> do (f, s1) <- apply p1 s
                          (x, s2) <- apply p2 s1
                          return (f x, s2))

-- Control.Applicative.some :: Alternative f => f a -> f [a]
-- Control.Applicative.many :: Alternative f => f a -> f [a]
-- Data.Foldable.asum :: (Foldable t, Alternative f) => t (f a) -> f a
instance Control.Applicative.Alternative Parser where
  empty = P (\s -> [])
  p1 <|> p2 = P (\s -> case apply p1 s
                       of [] -> apply p2 s
                          xs -> xs)

-- Control.Monad.liftM :: Monad m => (a1 -> r) -> m a1 -> m r
-- Control.Monad.liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
-- Control.Monad.liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
instance Monad Parser where
  return = pure
  p >>= f = P (\s1 -> concat (map (\(x,s2) -> apply (f x) s2) (apply p s1)))

-- Control.Monad.mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a
-- Control.Monad.msum :: MonadPlus m => [m a] -> m a
-- Control.Monad.guard :: MonadPlus m => Bool -> m ()
instance Control.Monad.MonadPlus Parser where
  mzero = Control.Applicative.empty
  mplus p1 p2 = P (\s -> apply p1 s ++ apply p2 s) 

-----------------
-- Fondamental --
-----------------

item :: Parser Char
item = P f where f [] = []
                 f (c:cs) = [(c,cs)]

fromRead :: Read a => Parser a
fromRead = P reads

-------------
-- Derived --
-------------

sat :: (Char -> Bool) -> Parser Char
sat f = Control.Monad.mfilter f item

char :: Char -> Parser ()
char c = sat (==c) >> return ()

string :: String -> Parser ()
string (c:cs) = char c >> string cs
string [] = return ()

blank :: Parser ()
blank = Control.Applicative.many (sat (`elem` [' ', '\t', '\n'])) >> return ()

keyword :: String -> Parser ()
keyword s = blank >> string s
