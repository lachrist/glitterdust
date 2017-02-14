
module Store(Store, Address, new, add, get, set, nxt) where

newtype Store a = S [a] deriving (Show, Eq)

newtype Address a = A Int deriving (Show, Eq)

new :: Store a
new = S []

add :: a -> Store a -> Store a
add x (S xs) = S $ xs ++ [x]

nxt :: Store a -> Address a
nxt (S xs) = A $ length xs

set :: Address a -> a -> Store a -> Store a  
set (A i) x1 (S xs) = S $ loop xs i
  where loop (_:xs)  0 = x1 : xs
        loop (x2:xs) i = x2 : loop xs (i-1)

get :: Address a -> Store a -> a
get (A i) (S xs) = xs!!i
