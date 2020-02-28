{-

This rep is intended to clear the fog around two common Functors that we use
in Haskell by having you implement them yourself. It lays a good foundation
for diving deeper into functors.

- Implement your own version of Maybe
- Implement map for your Maybe
- Create Functor instance for your Maybe

- Implement your own version of List
- Implement map for your list
- Create Functor instance for your List

-}
module DemystifyingFunctor where

data MyMaybe a
  = MyJust a
  | MyNothing

mapMyMaybe :: (a -> b) -> MyMaybe a -> MyMaybe b
mapMyMaybe f myMaybe =
  case myMaybe of
    MyJust a ->
      MyJust (f a)

    MyNothing ->
      MyNothing

instance Functor MyMaybe where
  fmap =
    mapMyMaybe

data MyList a
  = MyEmptyList
  | MyCons a (MyList a)

mapMyList :: (a -> b) -> MyList a -> MyList b
mapMyList f myList =
  case myList of
    MyEmptyList ->
      MyEmptyList

    MyCons a restOfMyList ->
      MyCons (f a) (mapMyList f restOfMyList)

