module Cards where

-- Datenmodell für Spielkarten (Farbe + Wert, Suit + Rank)
-- Datentyp Card: no illegal states

-- :info Ord

-- Liste aller Karten (mit Hilfsfunktion? nicht alle aufzählen)

-- Funktion, die prüft, ob eine Karte eine andere schlägt
-- (Gedanken über Rückgabewert)
-- cardBeats :: Card -> Card -> ???

data Suit = Diamonds | Hearts | Spades | Clubs
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
         | Jack | Queen | King | Ace
  deriving (Show, Eq, Ord, Enum, Bounded)

-- suitFromIndex :: Int -> Suit

-- map suitFromIndex [0..3]

-- mit Enum und Bounded:
allSuits :: [Suit]
allSuits = [minBound .. maxBound]

allRanks :: [Rank]
allRanks = [minBound .. maxBound]

data Card = MkCard Suit Rank

allCards :: [Card]
allCards = [ MkCard suit rank | suit <- allSuits, rank <- allRanks ]