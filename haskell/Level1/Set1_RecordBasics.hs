{-

Basics of Haskell Records

- Create a record with two fields of different types
- write a function that will create an instance of your record using record syntax
- write down the type of the data constructor for your record
- write a function that will create an instance of your record using regular data constructor syntax
- write a function that will update one of the fields of an existing record with a new value

-}
module Level1.Set1_RecordBasics where

data Moomoo =
  Moomoo
    { mooName :: String
    , mooAge  :: Int
    }

buildAsRecord :: String -> Int -> Moomoo
buildAsRecord name age =
  Moomoo
    { mooName = name
    , mooAge  = age
    }

moomooConstructor :: String -> Int -> Moomoo
moomooConstructor =
  Moomoo

buildFromConstructor :: String -> Int -> Moomoo
buildFromConstructor name age =
  Moomoo name age

moomooSetName :: String -> Moomoo -> Moomoo
moomooSetName newName moomoo =
  moomoo
    { mooName = newName
    }
