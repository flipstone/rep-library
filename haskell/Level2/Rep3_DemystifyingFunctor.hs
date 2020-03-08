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

- Write out a sample value using your types that is Maybe a List of Strings
- Write out a sample value using your types that is a List of Maybe Strings

- Write a function to get the length of all the strings in your Maybe List of Strings
  - Once using the mapping fuctions you wrote for you types explicitly
  - Once using the Functor instances via fmap

- Write a function to get the length of all the strings in your List of Maybe Strings
  - Once using the mapping fuctions you wrote for you types explicitly
  - Once using the Functor instances via fmap

- Finally, write a function that can get the length of all the strings in either
  one of the values above by using the Functor instance of both your types.

-}
module Level2.Rep3_DemystifyingFunctor where

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

instance Functor MyList where
  fmap =
    mapMyList

maybeListOfStrings :: MyMaybe (MyList String)
maybeListOfStrings =
  MyJust (MyCons "Bob" (MyCons "April" MyEmptyList))

listOfMaybeStrings :: MyList (MyMaybe String)
listOfMaybeStrings =
  MyCons (MyJust "April") (MyCons MyNothing (MyCons (MyJust "Bob") MyEmptyList))

lengthOfStrings1 :: MyMaybe (MyList String) -> MyMaybe (MyList Int)
lengthOfStrings1 strings =
  mapMyMaybe (mapMyList length) strings

lengthOfStrings2 :: MyMaybe (MyList String) -> MyMaybe (MyList Int)
lengthOfStrings2 strings =
  fmap (fmap length) strings

lengthOfStrings3 :: MyList (MyMaybe String) -> MyList (MyMaybe Int)
lengthOfStrings3 strings =
  mapMyList (mapMyMaybe length) strings

lengthOfStrings4 :: MyList (MyMaybe String) -> MyList (MyMaybe Int)
lengthOfStrings4 strings =
  fmap (fmap length) strings

lengthOfStrings5 :: (Functor f, Functor g) => f (g String) -> f (g Int)
lengthOfStrings5 strings =
  fmap (fmap length) strings
