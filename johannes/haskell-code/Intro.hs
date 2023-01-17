{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
-- x :: Integer = 7
x = 7

y :: Integer
-- >>> y
y = x * 12 + 13

f :: Integer -> Integer
-- >>> f 5
-- Merksatz: Funktionsapplikation bindet immer am stärksten
f = \ n -> n * 2

-- Abkürzung:
-- >>> f' 5
-- 10
f' n = n * 2

-- Ein Haustier ist eins der folgenden:
-- - Katze ODER
-- - Hund ODER
-- - Schlange
-- ==> Datendefinition ==> neuer Typ
-- >>> Cat
-- Cat
data Pet = Cat | Dog | Snake
  deriving (Show, Eq)

-- Haustiere vergleichen
-- Jede Funktion in Haskell hat genau ein Argument
-- Funktionen sind "total" -> zu jedem Eingang muss es Ergebnis geben
eqPet :: Pet -> (Pet -> Bool)
eqPet Cat Cat = True
eqPet Dog Dog = True
eqPet Snake Snake = True
eqPet _ _ = False -- <- "don't care"

-- instance Eq Pet where
--     -- Infix-Operatoren werden geklammert bei der Definition
--     (==) = eqPet

-- >>> Cat == Dog
-- False

-- Gürteltier hat folgende Eigenschaften
-- - lebendig oder tot UND
-- - Gewicht
data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

-- Record
--   v Typkonstruktor
--            v Datenkonstruktor
-- data Dillo = MkDillo { dilloLiveness :: Liveness,
--                        dilloWeight :: Weight }
--     deriving Show

dillo1 = MkDillo Alive 5
dillo3 = MkDillo Alive
-- >>> MkDillo { dilloLiveness = Dead }
-- alternative Schreibweise
-- Reihenfolge egal
dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 8 }

-- runOverDillo :: Dillo -> Dillo
-- variablen: kleingeschrieben
-- Record-Update-Syntax: gleicher Dillo, mit geändertem Wert
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 5}
-- runOverDillo dillo = dillo { dilloLiveness = Dead }
-- runOverDillo (MkDillo { dilloLiveness = liveness, dilloWeight = w }) =
-- runOverDillo (MkDillo { dilloLiveness = _, dilloWeight = w }) =
-- runOverDillo (MkDillo { dilloWeight = w }) =
--     MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- runOverDillo (MkDillo Alive w) = MkDillo Dead w
-- runOverDillo dillo = dillo

-- Algebraische Datentypen
-- Ein Tier ist eins der folgenden:
-- - Dillo mit Liveness und Gewicht
-- - Papagei mit Satz und Gewicht

type Sentence = String

-- alternative Modellierung:
-- data Dillo = MkOldDillo Liveness Weight
-- -- MkDillo (MkOldDillo Alive 5)

-- mkNewDillo :: Liveness -> Weight -> Animal
-- mkNewDillo l w = MkDillo (MkOldDillo l w)

data Animal
    -- Konstruktoren
    = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
    -- = MkDillo Dillo
    | MkParrot Sentence Weight
    deriving Show

parrot1 = MkParrot "hello" 3
parrot2 = MkParrot "bye" 1

-- Tiere überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ w) = MkParrot "" w

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
-- >>> feedAnimal (MkDillo Alive 2) 3
-- MkDillo {dilloLiveness = Alive, dilloWeight = 5}
-- feedAnimal (MkDillo Dead weight) amount = MkDillo Dead weight
-- feedAnimal (MkDillo Alive weight) amount = MkDillo Alive (weight + amount)
-- feedAnimal dillo@(MkDillo liveness weight) amount =
feedAnimal dillo@(MkDillo{}) amount =
    -- Fallunterscheidung in Haskell
    case dilloLiveness dillo of
        Dead -> dillo
        -- Alive -> MkDillo liveness (weight + amount)
        -- mit Update-Syntax:
        Alive -> dillo { dilloWeight = amount + dilloWeight dillo }
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight + amount)

--              v Tupel aus Weight und Animal
-- >>> (3, MkDillo Alive 3)
-- (3,MkDillo {dilloLiveness = Alive, dilloWeight = 3})
feedAnimal' :: (Animal, Weight) -> Animal
-- feedAnimal' (animal, w) = feedAnimal animal w
feedAnimal' = tuplify feedAnimal

-- eingebaut uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- Pattern Matching in Lambda
tuplify f = \ (a, b) -> f a b

-- eingebaut als curry + schönfinkeln
untuplify :: ((a, b) -> c) -> a -> b -> c
untuplify f = \ a b -> f (a, b)
-- untuplify f a b = f (a, b)
-- untuplify f a = \ b -> f (a, b)

multWith :: Int -> Int -> Int
multWith = (*)

-- Duschprodukte:
-- - Seife mit ph-Wert
-- - Shampoo mit Haartyp
-- - Duschgel (50% Seife, 50% Shampoo)

-- 1. Datenanalyse + Datentyp
-- 2. Funktion, die den Seifenanteil berechnet

-- 3. Erweiterung:
-- - Mixtur aus zwei Duschprodukten, bel. Anteile
-- Funktion entsprechend anpassen