{--
  Expressing Encoding

  - Define regular ADT for the TaggedInitial encoding of allowing for:
    * String constants
    * String concatenation
    * Integer consants
    * Incrementing Integers

  - Define a result type to hold a result String or Integer
  - Define evalTaggedInitial to evaluate an the TaggedInitial encoding DSL

  - Use GADTs to define TaglessInitial encoding of the same DSL
  - Define evalTaglessInitial

  - Define a typeclass FinalString for DSL expressions of the String operations above
  - Define a typeclass FinalInteger for DSL expressions of the Integer operations above

  (The type valiable in these typeclasses with be the type representing the
  expression itself)

  - Define a trivial FinalTagless newtype wrapper with an eval function to extract
    the value

  - Provide a FinalString instance for the FinalTagless type
  - Provide a FinalInteger instance for the FinalTagless type

  - Demonstrate the usage of FinalTagless a expression that evaluates to a String
  - Demonstrate the usage of FinalTagless a expression that evaluates to an Integer
--}
{-# LANGUAGE GADTs #-}
module LevelX.ExpressionEncoding where

data TaggedInitial
  = TaggedString String
  | TaggedInteger Integer
  | TaggedConcat TaggedInitial TaggedInitial
  | TaggedIncrement TaggedInitial

data TaggedResult
  = TaggedResultString String
  | TaggedResultInteger Integer

evalTaggedInitial :: TaggedInitial -> Maybe TaggedResult
evalTaggedInitial expr =
  case expr of
    TaggedString string ->
      pure $ TaggedResultString string

    TaggedInteger int ->
      pure $ TaggedResultInteger int

    TaggedConcat exprA exprB -> do
      resultA <- evalTaggedInitial exprA
      resultB <- evalTaggedInitial exprB
      case (resultA, resultB) of
        (TaggedResultString strA, TaggedResultString strB) ->
          pure $ TaggedResultString (strA ++ strB)

        _ ->
          Nothing

    TaggedIncrement subExpr -> do
      result <- evalTaggedInitial subExpr
      case result of
        TaggedResultInteger int ->
          pure $ TaggedResultInteger (int + 1)

        _ ->
          Nothing


data TaglessInitial a where
  TaglessString :: String -> TaglessInitial String
  TaglessInteger :: Integer -> TaglessInitial Integer
  TaglessConcat :: TaglessInitial String -> TaglessInitial String -> TaglessInitial String
  TaglessIncrement :: TaglessInitial Integer -> TaglessInitial Integer

evalTaglessInitial :: TaglessInitial a -> a
evalTaglessInitial expr =
  case expr of
    TaglessString string ->
      string

    TaglessInteger int ->
      int

    TaglessConcat exprA exprB ->
      evalTaglessInitial exprA ++ evalTaglessInitial exprB

    TaglessIncrement subExpr ->
      evalTaglessInitial subExpr + 1

class FinalString repr where
  finalString :: String -> repr String
  finalConcat :: repr String -> repr String -> repr String

class FinalInteger repr where
  finalInteger :: Integer -> repr Integer
  finalIncrement :: repr Integer -> repr Integer

newtype FinalTagless a
  = FinalTagless { evalFinalTagless :: a }
  deriving Show

instance FinalString FinalTagless where
  finalString =
    FinalTagless

  finalConcat a b =
    FinalTagless $
      evalFinalTagless a ++ evalFinalTagless b

instance FinalInteger FinalTagless where
  finalInteger =
    FinalTagless

  finalIncrement a =
    FinalTagless $
      evalFinalTagless a + 1

aStr :: String
aStr =
  evalFinalTagless $
    finalConcat
      (finalString "Hello")
      (finalConcat (finalString " ")
                   (finalString "World"))

anInteger :: Integer
anInteger =
  evalFinalTagless $
    finalIncrement
      (finalIncrement
        (finalInteger 10))
