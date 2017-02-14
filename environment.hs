
module Environment (Environment(Root), Store, Address, ext, def, get, set) where

import qualified Control.Applicative
import qualified Control.Monad
import qualified Expression
import qualified Store
import qualified Data.Maybe

type Key = Expression.Variable
type Store v = Store.Store (Environment v)
type Address v = Store.Address (Environment v)

data Environment v = Frame [(Key, v)] (Address v)
                   | Root
                   deriving (Show, Eq)

update :: Eq k => k -> v -> [(k,v)] -> Maybe [(k, v)]
update k1 v1 [] = Nothing
update k1 v1 ((k2,v2):xs) = if k1 == k2
                            then Just ((k1,v1):xs)
                            else Control.Monad.liftM ((:) (k2,v2)) (update k1 v1 xs)

ext :: [Key] -> [v] -> Address v -> Environment v
ext ks vs a = Frame (zip ks vs) a

def :: Key -> v -> Address v -> Store v -> Store v
def k1 v1 a1 s = let (Frame xs a2) = Store.get a1 s
                 in Store.set a1 (Frame (Data.Maybe.fromMaybe ((k1,v1):xs) (update k1 v1 xs)) a2) s

get :: Key -> Address v -> Store v -> Maybe v
get k a1 s = case Store.get a1 s
             of Root -> Nothing
                (Frame xs a2) -> lookup k xs Control.Applicative.<|> get k a2 s      

set :: Key -> v -> Address v -> Store v -> Maybe (Store v)
set k1 v1 a1 s = case Store.get a1 s
                 of Root -> Nothing
                    (Frame xs a2) -> maybe (set k1 v1 a2 s)
                                           (\xs -> Just $ Store.set a1 (Frame xs a2) s)
                                           (update k1 v1 xs)
