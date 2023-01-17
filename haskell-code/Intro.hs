{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intro where

import Prelude hiding (Monoid, Semigroup)

x :: Integer
x = 7

y :: Integer
-- >>> y
-- 97
y = x * 12 + 13

f :: Integer -> Integer
f = \n -> n * 2

-- Abkürzung
f' :: Integer -> Integer
f' 42 = 42
f' n = n * 2

-- Ein Haustier ist eins der folgenden
-- - Katze
-- - Hund
-- - Schlange

-- >>> Cat
-- Cat
data Pet = Cat | Dog | Snake
  deriving (Show, Eq)

eqPet :: Pet -> Pet -> Bool
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Snake Snake = True
eqPet _ _ = False

eqCat = eqPet Cat

-- Highway
data Liveness = Alive | Dead
  deriving (Show, Eq)

type Weight = Integer

-- data Dillo = MkDillo {dilloLiveness :: Liveness, dilloWeight :: Weight} deriving (Show, Eq)

dillo1 = MkDillo Alive 5

dillo2 = MkDillo Dead 8

runOverDillo dillo = dillo {dilloLiveness = Dead}

-- ADTs

type Sentence = String

data Animal
  = MkDillo {dilloLiveness :: Liveness, dilloWeight :: Weight}
  | MkParrot Sentence Weight
  deriving (Show, Eq)

parrot1 = MkParrot "Hello" 3

parrot2 = MkParrot "Bye" 1

parrot3 = MkParrot "Bye" 4

-- Tiere überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- Tiere füttern
-- >>> feedAnimal (MkDillo Alive 2) 12
-- MkDillo {dilloLiveness = Alive, dilloWeight = 14}
feedAnimal :: Animal -> Weight -> Animal
feedAnimal dillo@(MkDillo liveness weight) amount =
  case liveness of
    Dead -> dillo
    Alive -> MkDillo Alive (weight + amount)
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight + amount)

-- tuplify
tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f = \(a, b) -> f a b

-- Duschprodukte:
-- - Seife mit ph-Wert
-- - Shampoo mit Haartyp
-- - Duschgel (50% Seife, 50% Shampoo)

-- 1. Datenanalyse + Datentyp
-- 2. Funktion, die den Seifenanteil berechnet
-- 3. Erweiterung:
-- - Mixtur aus zwei Duschprodukten, bel. Anteile
-- Funktion entsprechend anpassen

-- -----

data Optional a
  = Result a
  | Null
  deriving (Show)

data Optional2 a
  = Result2 a
  | Null2
  deriving (Show)

class Semigroup a where
  -- muss gelten: Assoziativgesetz
  op :: a -> a -> a

-- String ist Synonym für [Char]
-- Listen bilden eine Halbgruppe
instance Semigroup [a] where
  op s1 s2 = s1 ++ s2

instance Semigroup Int where
  op = (+)

-- >>> op (2 :: Int) 3
-- 5
-- >>> op "abc" "def"
-- "abcdef"

-- Haskell hat mehrere Stringtypen
-- String, Text, LazyText, ByteString, Lazy ByteString
-- foo :: Text -> Text -> Text
-- bei Verwendung von `op` -> refactoringsicher
-- foo s t = s `op` t `op` "abc"

-- Monoid ist Spezialisierung von Semigroup
class Semigroup a => Monoid a where
  --  muss gelten: Neutralität von neutral
  -- op neutral x == x == op x neutral f.a. x
  neutral :: a

instance Monoid [a] where
  neutral = []

-- Tupel: (a, b)
instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  op :: (Semigroup a, Semigroup b) => (a, b) -> (a, b) -> (a, b)
  op (x, y) (a, b) = (x `op` a, y `op` b)

instance (Monoid a, Monoid b) => Monoid (a, b) where
  neutral = (neutral, neutral)

-- Übung:
-- Instanzen
-- - Semigroup (Optional a)
-- - Monoid (Optional a)

instance Semigroup a => Semigroup (Optional a) where
  op Null _ = Null
  op _ Null = Null
  op (Result r1) (Result r2) = Result (op r1 r2)

-- >>> op (Result (2 :: Int)) (Result 4)
-- Result 6

-- >>> op (Result (2 :: Int)) Null
-- Null

instance Monoid a => Monoid (Optional a) where
  neutral = Result neutral

-- >>> op (Result [1,2,3]) neutral
-- Result [1,2,3]

-- >>> op neutral (Result [1,2,3])
-- Result [1,2,3]

instance Semigroup a => Semigroup (Optional2 a) where
  op (Result2 r1) (Result2 r2) = Result2 (op r1 r2)
  op Null2 o = o
  op o Null2 = o

-- >>> op (Result2 (2 :: Int)) (Result2 4)
-- Result2 6

-- >>> op (Result2 (2 :: Int)) Null2
-- Result2 2

instance Semigroup a => Monoid (Optional2 a) where
  neutral = Null2

-- >>> op (Result2 [1,2,3]) neutral
-- Result2 [1,2,3]

-- >>> op neutral (Result2 [1,2,3])
-- Result2 [1,2,3]
