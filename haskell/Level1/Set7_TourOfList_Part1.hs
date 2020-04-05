{-

These reps will help build you knowledge about how lists work and some of the
functions available in the List API. List has far too many functions to cover
in a single set of reps, so we'll have to revisit List again in the future to
see some more advanced functions.

Note: Many of the list functions we use here are re-exported but the standard
Haskell Prelude, but some are not. You'll need to import Data.List to do some
of these reps. Also, some of these functions actually work with more types than
just List, but we're not going to worry about that for now.

- Implement a function, `safeHead`, that is like `head`, but produces `Maybe a`
  when the list is empty rather than raising an nasty runtime error.

- Implement a function, `safeTail`, that is like `tail`, but produces `Maybe [a]`
  when the list is empty instead of raising an error.

- Implement a function, `safeLast`, that is like `last`, but produces `Maybe a`
  when the list is empty instead of raising an error.

Note: The moral of these three functions is that you should really never
use `head`, `tail` and `last` because they can produce a runtime error and
there is almost always a better way.

- Construct a list of names of various lengths

- Use the list functions `map` and `list` to build a list of all the lengths of
  the names.

- Use `sum` to find the total lengths of all the names together.

- Use `intercalate` to constructor a printable roster of all the names in
  the list separated by a comma and space.

- Write a function to determine if a name is long (longer than 7 characters)

- Use `and` and `map` to determine if all the names in your list are long
- Use `all` to do the same thing, without needing to use `map`

- Use `or` and `map` to determine if any of the names in you list are long
- Use `any` to do the same thing, without needing `map`

- Use `filter` to build a list of just the long names

- Use `partition` to separate the list of names into long and short names in
  a single pass.

Note: The partition example below demonstrates that you can bind multiple top
level values at the same time by pattern matching on a tuple. Although this is
rarely used in real life, it's good to know. Otherwise it's might be extremely
confusing should you happen upon an example in the wild.

- Pick one of the example names and use `elem` to see if is in the list of
  long names.

- Use `find` to pick out the first long name from the list.

- Write a function to shorten a name to make it short using `take`
- Write a function to find what would remainder would be lost by shortening using `drop`.
- Write a function to shorten a name and return the remainder simultaneous using `splitAt`.

- Use `elem` write a function `isEnglishVowel` to check whether a character is
  an English vowel.

- Write a function to remove the leading vowels from a name using `dropWhile`
- Write a function to extract the leading vowels from a name using `takeWhile`
- Write a function to split off the leading vowels and return the remainder together using `span`
- Write a function to split off the leading consonants and return the remainder together using `break`

-}
module Level1.Set7_TourOfList_Part1 where

import qualified Data.List as List

safeHead :: [a] -> Maybe a
safeHead as =
  case as of
    [] ->
      Nothing

    a : _ ->
      Just a

safeTail :: [a] -> Maybe [a]
safeTail as =
  case as of
    [] ->
      Nothing

    _ : rest ->
      Just rest

safeLast :: [a] -> Maybe a
safeLast as =
  case as of
    [] ->
      Nothing

    [a] ->
      Just a

    _ : rest ->
      safeLast rest

names :: [String]
names =
  [ "Danielle", "Darryl", "Dartania", "Darcy", "Duke", "David" ]

lengthsOfNames :: [Int]
lengthsOfNames =
  map length names

totalLengthOfNames :: Int
totalLengthOfNames =
  sum lengthsOfNames

roster :: String
roster =
  List.intercalate ", " names

isLongName :: String -> Bool
isLongName name =
  length name > 7

areAllNamesLong1 :: Bool
areAllNamesLong1 =
  and (map isLongName names)

areAllNamesLong2 :: Bool
areAllNamesLong2 =
  all isLongName names

anyNameIsLong1 :: Bool
anyNameIsLong1 =
  or (map isLongName names)

anyNameIsLong2 :: Bool
anyNameIsLong2 =
  any isLongName names

justLongNames :: [String]
justLongNames =
  filter isLongName names

longNames, shortNames :: [String]
(longNames, shortNames) =
  List.partition isLongName names

isDarrylLong :: Bool
isDarrylLong =
  "Darryl" `elem` longNames

aLongName :: Maybe String
aLongName =
  List.find isLongName names

shortenName :: String -> String
shortenName =
  take 7

remainingName :: String -> String
remainingName =
  drop 7

shortenNameWithRemainder :: String -> (String, String)
shortenNameWithRemainder =
  splitAt 7

isEnglishVowel :: Char -> Bool
isEnglishVowel char =
  char `elem` "AaEeIiOoUu"

dropLeadingVowels :: String -> String
dropLeadingVowels =
  dropWhile isEnglishVowel

takeLeadingVowels :: String -> String
takeLeadingVowels =
  takeWhile isEnglishVowel

splitOffLeadingVowels :: String -> (String, String)
splitOffLeadingVowels =
  span isEnglishVowel

splitOffLeadingConsonants :: String -> (String, String)
splitOffLeadingConsonants =
  break isEnglishVowel
