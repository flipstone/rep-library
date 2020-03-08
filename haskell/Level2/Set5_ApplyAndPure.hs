{-

Apply and Pure:

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

- Build a record to represent a pet with three fields: name, age, and species.

- Create a Maybe value for each of the types of these three fields
- Use your `applyMaybe` function to build a `Maybe Pet` from these three maybe value.
  - Once using regular function application (with `applyMaybe` in front)
  - A second time using `applyMaybe` as an infix function.

- Evaluate your `Maybe Pet` value at the repl and make sure you understand
  the result. What happens if any of the values is `Nothing`?

- Create three lists of values -- one for each of the pet fields.

- Use your `applyList` function (in infix notation) to build a list of pets
  from tese values

- Evaluate your list of pets at the repl and make sure you understand the the
  result.

- Build another Maybe Pet value from your 3 Maybe values using the Functor
  and Applicative operators <$> and <*>

- Build another [Pet] value from your 3 List values using <$> and <*>

-}
module Level2.Set5_ApplyAndPure where

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

data Pet =
  Pet
    { petName :: String
    , petAge :: Int
    , petSpecies :: Species
    } deriving Show

data Species
  = Cat
  | Dog
  deriving Show

maybeName :: Maybe String
maybeName =
  Just "Sparkles"

maybeAge :: Maybe Int
maybeAge =
  Just 3

maybeSpecies :: Maybe Species
maybeSpecies =
  Just Cat

maybePet1 :: Maybe Pet
maybePet1 =
  applyMaybe (applyMaybe (fmap Pet maybeName) maybeAge) maybeSpecies

{-
   Produces:
    Just (Pet {petName = "Sparkles", petAge = 3, petSpecies = Cat})
-}

maybePet2 :: Maybe Pet
maybePet2 =
  fmap Pet maybeName
    `applyMaybe` maybeAge
    `applyMaybe` maybeSpecies

possibleNames :: [String]
possibleNames =
  ["Spot", "Dr. Doom"]

possibleAges :: [Int]
possibleAges =
  [4,5]

possibleSpecies :: [Species]
possibleSpecies  =
  [Dog, Cat]

possiblePets1 :: [Pet]
possiblePets1 =
  fmap Pet possibleNames
     `applyList` possibleAges
     `applyList` possibleSpecies

{-
   Produces:
      [ Pet {petName = "Spot", petAge = 4, petSpecies = Dog}
      , Pet {petName = "Spot", petAge = 4 , petSpecies = Cat}
      , Pet {petName = "Spot", petAge = 5, petSpecies = Dog}
      , Pet {petName = "Spot", petAge = 5, petSpecies = Cat}
      , Pet {petName = "Dr. Doom", petAge = 4, petSpecies = Dog}
      , Pet {petName = "Dr. Doom", petAge = 4, petSpecies = Cat}
      , Pet {petName = "Dr. Doom", petAge = 5, petSpecies = Dog}
      , Pet {petName = "Dr. Doom", petAge = 5, petSpecies = Cat}
      ]
-}

maybePet3 :: Maybe Pet
maybePet3 =
  Pet
    <$> maybeName
    <*> maybeAge
    <*> maybeSpecies

possiblePets2 :: [Pet]
possiblePets2 =
  Pet
    <$> possibleNames
    <*> possibleAges
    <*> possibleSpecies
