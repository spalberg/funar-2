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
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
    deriving Show

dillo1 = MkDillo Alive 5
dillo3 = MkDillo Alive
-- >>> MkDillo { dilloLiveness = Dead }
-- alternative Schreibweise
-- Reihenfolge egal
dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 8 }

runOverDillo :: Dillo -> Dillo
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
runOverDillo (MkDillo Alive w) = MkDillo Dead w
runOverDillo dillo = dillo