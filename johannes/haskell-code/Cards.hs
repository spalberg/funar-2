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