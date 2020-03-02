{-

These reps lay the groundwork for starting to talk about Applicative by making
sure the applicative functions for Maybe and List are deeply ingrained in your
neural pathways.

- Implement the pure function for Maybe
- Implement the pure function for List

- Implement the apply function for Maybe
- Implement the apply function for List by applying every function to every value

- Implement the zip function for List by apply the first function to the first value
and so forth

  * Note from the future: Notice that the signatures of applyList and zipList
    are the same. The pure function we wrote above has a special association
    with applyList, but not with zipList. More about this in the future.

-}
module ApplyAndPure where

pureMaybe :: a -> Maybe a
pureMaybe a =
  Just a

pureList :: a -> [a]
pureList a =
  [a]

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe maybeF maybeA =
  case maybeF of
    Nothing ->
      Nothing

    Just f ->
      case maybeA of
        Nothing ->
          Nothing

        Just a ->
          Just (f a)

applyList :: [a -> b] -> [a] -> [b]
applyList fs as =
  case fs of
    [] ->
      []

    (f : rest) ->
      fmap f as ++ applyList rest as

zipList :: [a -> b] -> [a] -> [b]
zipList fs as =
  case (fs, as) of
    (f : restOfFs, a : restOfAs) ->
      f a : zipList restOfFs restOfAs

    _ ->
      []
