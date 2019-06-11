
module State (StateT, State, apply, get, put) where

import qualified Control.Monad.Trans.Class
import qualified Control.Monad
import qualified Control.Applicative

newtype StateT s m a = ST (s -> m (a, s))

get :: Monad m => StateT s m s
get = ST (\s -> return (s, s))

put :: Monad m => s -> StateT s m ()
put s = ST (\_ -> return ((), s))

apply :: StateT s m a -> s -> m (a, s)
apply (ST f) s = f s

instance Control.Monad.Trans.Class.MonadTrans (StateT s) where
  lift m = ST (\s -> Control.Monad.liftM (\x -> (x,s)) m)

instance Functor m => Functor (StateT s m)  where
  fmap f t = ST $ fmap (\(x, s) -> (f x, s)) . apply t

-- Rule of thumb: if monadic computations are independant
-- (i.e. not based on the outcome of other monadic computation),
-- then we only need applicative instead of monad.
-- This does NOT mean that you can commute applicative computations!
-- Take state as applicative:
--
-- newtype State s a = S (s -> (a,s))
-- instance Applicative State where
--   pure x = S $ (\s -> (s,x))
--   m1 <*> m2 = S (\s -> let (f, s') = apply m1 s
--                            (x, s'') = apply m2 s'
--                        in (f x, s''))
--
-- Clearly, evaluating the computation m2 before m1 may lead to a different outcomes.
-- This explains why (Applicative m) is not enough to make (SateT s m) applicative.
-- The only way to rely only on (Applicative m) is to discard intermediary states:
--
-- instance (Monad m) => Applicative StateT s m) where
--   pure x = S $ (\s -> (s,x))
--   m1 <*> m2 = S (\s -> do  (f, _) <- apply m1 s
--                            (x, _) <- apply m2 s
--                            return (f x, s))
--
-- This last do block can be rewriten applicative style:
--
-- m1 <*> m2 = S $ \s -> liftM2 (\(f, _) (x, _) -> (f x, s)) (apply t1 s) (apply t2 s)
-- m1 <*> m2 = S $ \s -> liftA2 (\(f, _) (x, _) -> (f x, s)) (apply t1 s) (apply t2 s)
-- m1 <*> m2 = S $ \s -> pure (\(f, _) (x, _) -> (f x, s)) <*> apply t1 s <*> apply t2 s
--
-- However this implementation hardly makes sens...

instance Monad m => Applicative (StateT s m) where
  pure x = ST $ return . (,) x
  t1 <*> t2 = ST $ \s -> do (f, s') <- apply t1 s
                            (x, s'') <- apply t2 s'
                            return (f x, s'')

-- In the standard implementation, <|> is defined in terms of mplus, I have no idea why.
-- https://hackage.haskell.org/package/transformers-0.5.2.0/docs/src/Control.Monad.Trans.State.Lazy.html

instance (Monad m, Control.Applicative.Alternative m) => Control.Applicative.Alternative (StateT s m) where
  empty = ST $ const Control.Applicative.empty
  t1 <|> t2 = ST (\s -> apply t1 s Control.Applicative.<|> apply t2 s)

instance Monad m => Monad (StateT s m) where
  return x = ST $ return . (,) x
  t >>= f = ST (\s -> apply t s >>= (\(x, s') -> apply (f x) s'))

instance Control.Monad.MonadPlus m => Control.Monad.MonadPlus (StateT s m) where
  mzero = ST $ const Control.Monad.mzero
  mplus t1 t2 = ST (\s -> apply t1 s `Control.Monad.mplus` apply t2 s) 

----------------
-- Pure State --
----------------

newtype Identity a = I a

type State s a = StateT s Identity a

instance Functor Identity where
  fmap f (I x) = I $ f x

instance Applicative Identity where
  pure = I
  (I f) <*> (I x) = I $ f x

instance Monad Identity where
  return = I
  (I x) >>= f = f x
