module Cards where

import Data.Semigroup (Option)

-- Datenmodell für Spielkarten (Farbe + Wert, Suit + Rank)
-- Datentyp Card: no illegal states

-- Liste aller Karten (nicht alle aufzählen)

-- Funktion, die prüft, ob eine Karte gegen eine andere gewinnt

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

data Suit = Diamonds | Hearts | Spades | Clubs
  deriving (Show, Eq, Enum, Bounded)

data Card = Card Suit Rank
  deriving (Show, Eq)

allCards :: [Card]
allCards =
  let ranks = [minBound .. maxBound]
      suits = [minBound .. maxBound]
   in [Card suit rank | suit <- suits, rank <- ranks]

beats :: Card -> Card -> Maybe Bool
beats (Card s1 r1) (Card s2 r2) =
  if s1 == s2
    then Just (r1 > r2)
    else Nothing
