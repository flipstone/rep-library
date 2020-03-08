{-

- Write a function fmapList that does what fmap does for a List
- Write a function fmapMaybe that does what fmap does for a Maybe
- Write a function foldLList function that does what foldl does for a List

-}
module Level2.Rep1_DemystifyingPrelude where

fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe functionA2B ma =
  case ma of
    Nothing -> Nothing
    Just a -> Just $ functionA2B a

fmapList :: (a -> b) -> [a] -> [b]
fmapList functionA2B listA =
  case listA of
    [] -> []
    (x:xs) -> functionA2B x : fmapList functionA2B xs

foldLList :: (b -> a -> b) -> b -> [a] -> b
foldLList functionBA2B b listA =
  case listA of
    [] -> b
    (x:xs) ->
      foldLList functionBA2B (functionBA2B b x) xs
