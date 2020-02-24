{--
  Data.Ord / Sorting lists

  - define an enum for player rank and build a manual Ord instance for it
  - sort a list of ranks into ascending order
  - sort a list of ranks into descinding ordering using Ord.Down
  - define a Player record with name and rank
      - Sort a list of players alphabetically using sortBy / comparing
      - Sort a list of players by rank using sortOn
  - Build a compare function for players that compares by name and then rank
      - Hint: use Ord.comparing to compare the fields
      - build one that is fully explicit
      - Then build one that uses the Ordering Semigroup
      - Then build one that uses the Ordering Semigroup and the (->) Semigroup
--}
module Ord where

import qualified Data.List as List
import qualified Data.Ord as Ord

data Rank
  = Gold
  | Silver
  | Bronze
  deriving (Show, Eq)

instance Ord Rank where
  compare left right =
    case (left, right) of
      (Gold, Gold) -> EQ
      (Silver, Silver) -> EQ
      (Bronze, Bronze) -> EQ
      (Gold, _) -> GT
      (Silver, _) -> GT
      (Bronze, _) -> LT

ranks :: [Rank]
ranks =
  [ Silver, Gold, Bronze, Silver, Bronze, Gold ]

ascendingRanks :: [Rank]
ascendingRanks =
  List.sort ranks

descendingRanks :: [Rank]
descendingRanks =
  List.sortOn Ord.Down ranks

data Player =
  Player
    { playerName :: String
    , playerRank :: Rank
    } deriving (Show, Eq)

players :: [Player]
players =
  [ Player "Bob" Gold
  , Player "Jean" Bronze
  , Player "Samantha" Silver
  , Player "Zelda" Gold
  ]

alphabeticalPlayers :: [Player]
alphabeticalPlayers =
  List.sortOn playerName players

rankedPlayers :: [Player]
rankedPlayers =
  List.sortBy (Ord.comparing playerRank) players

comparePlayers1 :: Player -> Player -> Ordering
comparePlayers1 left right =
  case Ord.comparing playerRank left right of
    LT -> LT
    GT -> GT
    EQ -> Ord.comparing playerName left right

comparePlayers2 :: Player -> Player -> Ordering
comparePlayers2 left right =
  Ord.comparing playerRank left right <> Ord.comparing playerName left right

comparePlayers3 :: Player -> Player -> Ordering
comparePlayers3 =
  Ord.comparing playerRank <> Ord.comparing playerName
