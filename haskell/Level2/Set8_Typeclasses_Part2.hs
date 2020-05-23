{-

These reps will help you get more familiar with typeclass laws,
typeclasses with multiple member functions, and typeclass members with
default implementations.

Write the Stack typeclass definition with methods:

- push
- pop
- peek (with a default implementation that follows the law)

Write down the laws for the Stack typeclass

- Law of Push/Pop
- Law of Peek

Write an instance of Stack for List

- Implement push
- Implement pop

Write an instance of Stack for Maybe

- Implement push
- Implement pop

- Define SizedStack that stores items in a list, but
  has size attributes to enforce a max size

- Implement pushSizedStack
- Implement popSizedStack
- Implement peekSizedStack

- Write an instance of Stack for SizedStack

-}
module Level2.Set8_Typeclasses_Part2 where

import qualified Control.Monad as Monad
import qualified Numeric.Natural as Nat

{-
   Stack Laws

   Law of push/pop:

   If push a s == Just t
   then pop t == Just (a, s)

   Law of peek:

   peek s == fmap fst (pop s)
-}

class Stack stack where
  push :: a -> stack a -> Maybe (stack a)

  pop :: stack a -> Maybe (a, stack a)

  peek :: stack a -> Maybe a
  peek =
    fmap fst . pop

instance Stack [] where
  push a stack =
    Just (a : stack)

  pop stack =
    case stack of
      [] ->
        Nothing

      a : rest ->
        Just (a, rest)

instance Stack Maybe where
  push a stack =
    case stack of
      Nothing ->
        Just (Just a)

      Just _ ->
        Nothing


  pop stack =
    case stack of
      Nothing ->
        Nothing

      Just a ->
        Just (a, Nothing)

data SizedStack a =
  SizedStack
    { maxSize :: Nat.Natural
    , currentSize :: Nat.Natural
    , items :: [a]
    }

pushSizedStack :: a -> SizedStack a -> Maybe (SizedStack a)
pushSizedStack a stack = do
  Monad.guard (currentSize stack < maxSize stack)
  pure $
    stack
      { currentSize = currentSize stack + 1
      , items = a : items stack
      }

popSizedStack :: SizedStack a -> Maybe (a, SizedStack a)
popSizedStack stack =
  case items stack of
    [] ->
      Nothing

    a : rest ->
      let
        remainingStack =
          stack
            { currentSize = currentSize stack - 1
            , items = rest
            }

      in
        Just (a, remainingStack)

peekSizedStack :: SizedStack a -> Maybe a
peekSizedStack stack =
  case items stack of
    [] ->
      Nothing

    a : _ ->
      Just a

instance Stack SizedStack where
  push = pushSizedStack
  pop = popSizedStack
  peek = peekSizedStack

