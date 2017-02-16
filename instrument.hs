
import qualified Store
import qualified State
import qualified Control
import qualified Control.Monad
import qualified Primitive

type CAddress = Store.Address Control.Control
type CStore = Store.Store Control.Control

--------------
-- Builders --
--------------

register :: Control.Control -> State.State CStore CAddress
register c = do cs <- State.get
                State.put $ Store.add c cs
                return $ Store.nxt cs

list :: [CAddress] -> State.State CStore CAddress
list [] = register $ Control.Constant $ Primitive.wrap ()
list (a:as) = do a1 <- register $ Control.Get $ Control.V "cons"
                 a2 <- list as
                 register $ Control.Call a1 [a, a2]

trap :: String -> CAddress -> [CAddress] -> State.State CStore CAddress
trap s a as = do a1 <- register $ Control.Get $ Control.V s
                 a' <- register $ Control.Constant $ Primitive.wrap $ show a
                 register $ Control.Call a1 (as++[a'])

----------------
-- Instrument --
----------------

visit :: CAddress -> State.State CStore CAddress
visit a = State.get >>= instrument a . Store.get a

instrument :: CAddress -> Control.Control -> State.State CStore CAddress
instrument a (Control.Constant _)      = trap "$constant" a [a]
instrument a (Control.Get _)           = trap "$get" a [a]
instrument a (Control.Namespace)       = trap "$namespace" a [a]
instrument a (Control.Lambda vs a1)    = do a' <- (visit a1 >>= register . Control.Lambda vs)
                                            trap "$lambda" a [a']  
instrument a (Control.Call a1 as)      = do a1' <- visit a1
                                            as' <- Control.Monad.sequence $ map visit as
                                            as'' <- list as' 
                                            trap "$call" a [a1', as'']
instrument a (Control.Branch a1 a2 a3) = do a1' <- visit a1 >>= trap "$if" a  . (:[])
                                            a2' <- visit a2
                                            a3' <- visit a3
                                            register $ Control.Branch a1' a2' a3' 
instrument a (Control.Def v a1)        = (visit a1 >>= trap "$def" a . (:[])) >>= environment (Control.Def v)
instrument a (Control.Set v a1)        = (visit a1 >>= trap "$set" a . (:[])) >>= environment (Control.Set v)

-- (define x 1)
--
-- (begin
--   (define $ ($def ($constant 1)))
--   (define x (car $))
--   (cdr $))

environment :: (CAddress -> Control.Control) -> CAddress -> State.State CStore CAddress
environment f a = do a0 <- register $ Control.Get $ Control.V "begin"
                     a1 <- register $ Control.Def (Control.V "$") a
                     a2 <- do a1 <- register $ Control.Get $ Control.V "car"
                              a2 <- register $ Control.Get $ Control.V "$"
                              a3 <- register $ Control.Call a1 [a2]
                              register $ f a3
                     a3 <- do a1 <- register $ Control.Get $ Control.V "cdr"
                              a2 <- register $ Control.Get $ Control.V "$"
                              register $ Control.Call a1 [a2]
                     register $ Control.Call a0 [a1, a2, a3]
