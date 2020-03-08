{--
   Control.Applicative.Free

   - Create a Red / Black Tag type and a Tagged a wrapper to tag values
     - build smart constructors for values
     - provide a Functor instance for Tagged (iterAp will remind you if you forget)
   - Implement ban: convert Tagged a to Maybe a by banning values tagged with the given tag
   - Implement taggedPlus: Add two Tagged Int values using liftAp
   - Implement runBanned: Use runAp to evaluate an Ap Tagged a to a Maybe a, banning values with the given tag
   - Implement tags: Use runAp_ to collect a Set of Tags from an Ap Tagged a
   - Implement runPlain: Use iterAp to execute an Ap Tagged a by just extracting the value
   - Implement runBanned2: Another version of runBanned using only retractAp and hoistAp
--}

module LevelX.FreeApplicative where

import qualified Control.Applicative.Free as FreeAp
import qualified Control.Monad as Monad
import qualified Data.Set as Set

data Tag
  = Red
  | Black
  deriving (Show, Eq, Ord)

data Tagged a =
  Tagged Tag a
  deriving (Show, Eq)

instance Functor Tagged where
  fmap f (Tagged tag a) =
    Tagged tag (f a)

red :: a -> Tagged a
red = Tagged Red

black :: a -> Tagged a
black = Tagged Black

ban :: Tag -> Tagged a -> Maybe a
ban bannedTag (Tagged tag a) = do
  Monad.guard (tag /= bannedTag)
  pure a

taggedPlus :: Tagged Int -> Tagged Int -> FreeAp.Ap Tagged Int
taggedPlus a b =
  (+) <$> FreeAp.liftAp a <*> FreeAp.liftAp b

runBanned :: Tag -> FreeAp.Ap Tagged a -> Maybe a
runBanned bannedTag =
  FreeAp.runAp (ban bannedTag)

tags :: FreeAp.Ap Tagged a -> Set.Set Tag
tags =
  FreeAp.runAp_ extractTag
    where
      extractTag (Tagged tag _) =
        Set.singleton tag

runPlain :: FreeAp.Ap Tagged a -> a
runPlain =
  FreeAp.iterAp extractValue
    where
      extractValue (Tagged _ a) =
        a

runBanned2 :: Tag -> FreeAp.Ap Tagged a -> Maybe a
runBanned2 bannedTag =
  FreeAp.retractAp . FreeAp.hoistAp (ban bannedTag)
