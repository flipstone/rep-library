{--

Semigroup
  | Semigroups are types that "combine". They have an binary operation that
  | takes to items of the type and produces a result of the same type. That
  | operation must be associative. We often talk about Semigroups as
  | appending because that is a very common use case, but the operation is
  | not actually required to represent an append.

- Write out the Semigroup law

The following reps will help you remember what `<>` does for List, Ordering,
Maybe, a Function.

- Implement combineList1: actually implement appending for lists yourself
- Implement combineList2: Use a function from Data.List this time.
- Implement combineOrdering
- Implement combineMaybe (Using Semigroup of underlying type)
- Implement combineFunction (Using Semigroup of underlying type)

- Create a newtype around Int and implement Semigroup using addition
- Create a newtype around Int and implement Semigroup using multiplication

Monoid
  | Monoid are Semigroups for which there is a value that turns the Semigroup's
  | combining operation into the identify function. This value is called the
  | "identity" of the Semigroup operation, and is written as `mempty` in
  | Haskell. The Monoid laws express the notion of `mempty` being the
  | identity of the Semigroup.

- Write out the Monoid laws for mempty

These reps will help you remember what the Semigroup identity is for List,
Ordering, Maybe and Function.

- Implement memptyList
- Implement memptyOrdering
- Implement memptyMaybe
- Implement memptyFunction

- implement Monoid for your addition newtype from above
- implement Monoid for your multiplication newtype from above

--}

module Level2.Rep4_DemystifyingSemigroupAndMonoid where

{-
  Semigroup Law: Associativity

  (x <> y) <> z = x <> (y <> z)
-}

combineList1 :: [a] -> [a] -> [a]
combineList1 front back =
  case front of
    [] ->
      back

    (first : rest) ->
      first : combineList1 rest back

combineList2 :: [a] -> [a] -> [a]
combineList2 =
  (++)

combineOrdering :: Ordering -> Ordering -> Ordering
combineOrdering primary secondary =
  case primary of
    LT -> LT
    GT -> GT
    EQ -> secondary

combineMaybe :: Semigroup a => Maybe a -> Maybe a -> Maybe a
combineMaybe left right =
  case (left, right) of
    (Nothing, anything) ->
      anything

    (anything, Nothing) ->
      anything

    (Just someLeft, Just someRight)  ->
      Just (someLeft <> someRight)

combineFunction :: Semigroup b => (a -> b) -> (a -> b) -> a -> b
combineFunction f g a =
  f a <> g a

newtype Addition =
  Addition
    { getAddition :: Int
    }

instance Semigroup Addition where
  left <> right =
    Addition $
      getAddition left + getAddition right

newtype Multiplication =
  Multiplication
    { getMultiplication :: Int
    }

instance Semigroup Multiplication where
  left <> right =
    Multiplication $
      getMultiplication left * getMultiplication right

{-
  Monoid laws for mempty:
    x <> mempty = x
    mempty <> x = x
-}

memptyList :: [a]
memptyList =
  []

memptyOrdering :: Ordering
memptyOrdering =
  EQ

memptyMaybe :: Maybe a
memptyMaybe =
  Nothing

memptyFunction :: Monoid b => a -> b
memptyFunction _ =
  mempty
