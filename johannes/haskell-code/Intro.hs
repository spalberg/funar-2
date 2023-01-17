{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- explizite Exportliste
module Intro (x, f, Weight', Animal(..), makeWeight) where

import Prelude hiding (Semigroup, Monoid)

-- import Prelude hiding (Semigroup, Monoid)

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

data HairType = Straight | Wavy
type PhValue = Integer
type Ratio = Double

data ShowerProduct
    = Shampoo HairType
    | Soap PhValue
    | ShowerGel
    | Mixture ShowerProduct ShowerProduct Ratio
    -- Es ist gesichert, dass ratio1 + ratio2 = 100
    -- | Mixture ShowerProductWithRatio ShowerProductWithRatio
    -- | Mixture [(ShowerProduct, Ratio)]

-- Make illegal states unrepresentable

-- >>> Mixture (Shampoo Straight) (Mixture (MkSoap 5) ShowerGel 30) 50

data ShowerProductWithRatio = MkShowerProductWithRatio Ratio ShowerProduct

-- data Mixture = MkMixture ...

calculateSoap :: ShowerProduct -> Ratio
calculateSoap (Shampoo{}) = 0
calculateSoap (Soap{}) = 100
calculateSoap (ShowerGel{}) = 50
calculateSoap (Mixture product1 product2 ratio) = undefined
    -- (calculateSoap product1) (calculateSoap product2)

-- type Weight = Int

-- data Weight' = MkWeight Int
-- newtype: keine Laufzeitkosten, da gleiche Repräsentation wie Int
newtype Weight' = MkWeight Int

makeWeight :: Int -> Optional Weight'
makeWeight x = if x < 0 then Null else Result (MkWeight x)

-- ganz typsicher, aber umständlich
-- data Nat = Zero | Succ Nat
-- three :: Nat
-- three = Succ (Succ (Succ Zero))

add2ToWeight :: Weight' -> Weight'
add2ToWeight (MkWeight weight) = MkWeight (weight + 2)

data ListOfInteger =
    -- die leere Liste
    Empty
    -- Cons-Liste aus erstem Element und Rest-Liste
    | Cons Integer ListOfInteger
    deriving Show

-- >>> list1
-- Cons 3 (Cons 5 Empty)
list1 = Cons 3 (Cons 5 Empty)

-- natürlich eingebaut
-- [a] :: Liste mit Inhaltstyp a
list2 :: [Integer]
list2 = [2,3,4]

-- : ist Cons (Konstruktor)
-- >>> 2 : 3 : 4 : []
-- [2,3,4]
-- >>> [] :: [String]
-- []

listSum :: [Integer] -> Integer
listSum [] = 0
--       v erstes Element
--            v Rest-Liste
listSum (x : xs) = x + listSum xs

listFold :: acc -> (a -> acc -> acc) -> [a] -> acc
listFold neutral op [] = neutral
--                              `` um Funktion -> Infix
listFold neutral op (x : xs) = x `op` (listFold neutral op xs)
-- >>> listFold 0 (+) [1,2,3]
-- 6
-- >>> listFold [] (:) [1,2,3,4]
-- [1,2,3,4]

-- lazy evaluation
-- strikte Auswertung (Racket):
-- - bei Funktionsaufruf werden erst Argumente ausgewertet
-- - hier: Argumente werden erst dann ausgewertet, wenn sie benötigt werden

natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

-- Vielfache einer Zahl aus Liste streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
-- gestern: extract. Haskell: filter
strikeMultiples n xs =
    filter (\ m -> mod m n /= 0) xs

-- Sieb des Eratosthenes
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x : xs) = x : (sieve (strikeMultiples x xs))

-- >>> head [1,2]
-- 1
-- >>> head []
-- Prelude.head: empty list

-- Algebraischer Datentyp mit Typvariable
-- "polymorph"
data Optional a
    = Result a
    | Null
    deriving Show

listIndex :: Integer -> [a] -> Optional a
listIndex _ [] = Null
listIndex n (x : xs) =
    if n == 0 then Result x else listIndex (n - 1) xs
-- >>> listIndex 2 []
-- Null
-- >>> listIndex 2 ["a", "b", "c"]
-- Result "c"

-- type String = [Char]
appendFooToFirstElement :: [String] -> Optional [String]
appendFooToFirstElement [] = Null
appendFooToFirstElement xs =
    -- lokale Variablen: let ... in ...
    let firstElement = listIndex 0 xs in
    case firstElement of
        Null -> Null
        Result x -> Result ((x ++ "foo") : tail xs)

-- Matthias: map :: (a -> b) -> [a] -> [b]
-- Praxis: map (7*) daten

-- wollen auf dem Typlevel _sehen_ können, ob und wie eine
-- Funktion fehlschlagen kann

-- Typklasse (denk: Interface)
-- class Eq a where
--     Methode:
--     (==) :: a -> a -> Bool

-- Implementierung / Instanz:
data Foo = Foo String

instance Show Foo where
    -- show :: Show a => a -> String
    show :: Foo -> String
    show (Foo s) = "SHOWING: " ++ s

-- Wann sollte man Typklassen implementieren/bauen?
-- -> Typklassen sind zuständig für universelle Abstraktionen!
-- Bisher: Show, Eq, Ord (totale Ordnungen)

-- Typklassen machen es möglich, die Domäne von polymorphen Funktionen
-- einzuschränken

showIfEqual :: (Eq a, Show a) => a -> a -> String
showIfEqual a x = if a == x then show x else ""

{-

Algebra:
- Typ T
- Operationen mit Signaturen
-- - Gesetze / Gleichungen

Plus:
- Typ T
- neutrales Element: n (denk 0)
- Operation: (op) :: T -> T -> T (denk +)
- op n x == op x n == x

(Monoid: falls assoziativ)

Halbgruppe:
- binäre Operation: op :: a -> a -> a
- assoziativ: op (op a b) c == op a (op b c)
- assoziativ: (a `op` b) `op` c == a `op` (b `op` c)
- jetzt sinnvoll: a `op` b `op` c

-}

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