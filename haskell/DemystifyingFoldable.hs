{-

Foldable represents a structure that, given a suitable "folding" function, can
be collapsed into a new value without the structure.

In these reps you'll practice the various ways Foldable can be implemented and
the how to implement it on different kinds of datastructures.

- Implement a type like Maybe
- Write a full instance of Foldable for your "Maybe"
- Write a newtype around List
- Write a full instance of Foldable for your "List"

-}


module DemystifyingFoldable where

data Option a
  = Something a
  | None

instance Foldable Option where
  foldr fn seed opt =
    case opt of
      Something a -> fn a seed
      None        -> seed

  foldMap fn opt =
    case opt of
      None        -> mempty
      Something a -> fn a

newtype Listy a = Listy [a]

instance Foldable Listy where
  foldr fn seed lst =
    case lst of
      Listy [] -> seed
      Listy (first : rest) -> foldr fn (fn first seed) (Listy rest)

  foldMap fn lst =
    case lst of
      Listy [] -> mempty
      Listy (first : rest) -> fn first <> foldMap fn rest
