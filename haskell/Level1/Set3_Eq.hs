{--

Eq
  | Eq is a typeclass for types that can be compared for equality.

- Write out the Eq "Laws"
  * Note that the Haskell Report that defines the official Haskell language
    does not give an official laws for Eq. Despite this, there are customary
    laws given in the docs for Eq that represent the general expected practice
    for implementing Eq. The laws given in the answer here are those, with
    one omission (Substitutivity), for simplicities sake.
  - Reflexivity
  - Symmetry
  - Transitivity
  - Negation

- Define an enum type and provide an Eq instance for it
- Define a record type and provide an Eq instance for it
- Define another enum type and derive Eq for it
- Define another record type and derive Eq for it

--}
module Level1.Set3_Eq where

{-
  Eq "laws"

  Reflexivity:
    x == x = True

  Symmetry:
    x == y = y == x

  Transitivity:
    If x == y and y == z then x == z

  Negation:
    x /= y = not (x == y)
-}

data Pies
  = Apple
  | Pumpkin
  | Rhubarb

instance Eq Pies where
  left == right =
    case (left, right) of
      (Apple, Apple) -> True
      (Pumpkin, Pumpkin) -> True
      (Rhubarb, Rhubarb) -> True
      _ -> False

data RecipeIngredient =
  RecipeIngredient
    { ingredientDescription :: String
    , ingredientAmount :: Integer
    }

instance Eq RecipeIngredient where
  left == right =
    ingredientDescription left == ingredientDescription right
    && ingredientAmount left == ingredientAmount right

data Cakes
  = Genoise
  | PoundCake
  | CarrotCake
  deriving Eq

data Frosting =
  Frosting
    { frostingFlavor :: String
    , frostingColor :: String
    } deriving Eq
