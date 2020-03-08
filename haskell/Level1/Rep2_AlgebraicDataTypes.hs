{-

Enumerated (enum) type
  Enum is a data type consisting of a set of named values.
  - Create a data that has three different Color constructors
  - Create a function that goes from your Color to a String of your color.

Basic Algebraic Data Type
  An algebraic data type (ADT) has one or more data constructors,
  and each data constructor can have zero or more arguments.
  - Create type to represent Vegetables. Celery has data arguments. Carrots
    need a color.
  - Create a function that takes a Vegetable and gives its Color.
  - Create a function that takes a Vegetable and give a String of the color.

Polymorphic Algebraic data type
  - Create a type called Keep that can story anything. Things can be kept in
    either a Shoebox or a Safe.
  - Create a function to take items out of the Keep.
  - Put a Red Carrot into a Shoebox(Keep) and write out what its type would be.
  - Create a Shoebox Carrot and write out what its type would be.
-}

module Level1.Rep2_AlgebraicDataTypes where

data Color
  = Green
  | Blue
  | Red

colorToString :: Color -> String
colorToString col =
  case col of
    Green -> "Green"
    Blue -> "Blue"
    Red -> "Red"

data Vegetable
  = Celery
  | Carrot Color

vegetableColor :: Vegetable -> Color
vegetableColor veg =
  case veg of
    Celery -> Green
    Carrot col -> col

vegetableToString :: Vegetable -> String
vegetableToString veg =
  colorToString $ vegetableColor veg

data Keep a
  = Shoebox a
  | Safe a

redCarrotShoebox :: Keep Vegetable
redCarrotShoebox = Shoebox $ Carrot Red

carrotShoebox :: Keep (Color -> Vegetable)
carrotShoebox = Shoebox Carrot
