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

data WGT = MkWGT Integer
