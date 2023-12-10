import Prelude hiding (map, foldl, Functor) -- we have to hide map, foldl and Functor, because we implement our own version in this code

-- Obfuscated Haskell code for fruit lovers

calculate :: a -> (a -> b) -> b -- looks like f : a -> b, f(a) = ?
calculate input function = function input   -- c x f = f x

ignore :: a -> b -> a
ignore input _ = input  -- just ignore the second input, whatever it might be.

-- Obfuscated Haskell code for animal lovers

-- Reversed Linked list type, where we have an Empty symbol and a not-Empty symbol,
-- which Stores the rest List and the value at the current position, not like in the build-in,
-- list type, where we first get the value and then the rest list (restlist ++ [value]) vs. (x : xs) basically.
data List a = Empty | Rest (List a) a

-- looks like a Rosetree where we say that every Node has a value.
-- so to speak we Store the value of type a and then a List of Nodes.
-- if this list is Empty, then we have a leaf. For the list we use the just defined type.
data Rosetree a = Brach a (List (Rosetree a))

-- type looks like map for our just defined data structure
map :: (a -> b) -> List a -> List b
map f Empty             = Empty                          -- Inductive start: Map the empty list to itself.
map f (Rest list value) = Rest (map f list) (f value)    -- Inductive step: Map the value and append to rest.

-- type looks like foldl for our just defined data structure but with swapped arguments
foldl :: a -> (a -> b -> a) -> List b -> a
foldl startVal f Empty                = startVal                        -- Inductive start: Map empty f val to val
foldl startVal f (Rest list value) = f (foldl startVal f list) value    -- Pass the starting value to the inductive start and concat with f-operation

class Functor a where            -- because of the comment below this one, this most likely is the Functor class
  fmap :: (b -> c) -> a b -> a c -- type signature looks like fmap

instance Functor List where -- Implementation of fmap for our List type
  fmap = map                -- Makes sense that map is the fmap implementation for the Functor for the List type
