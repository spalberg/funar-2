module DB where

import qualified Data.Map as Map -- <- Alias
-- müssen schreiben: Map.insert ...
-- ab und an nervt: m Map.! "johannes"
import Data.Map (Map, (!)) -- <- alles 

-- Datenbank: Key-Value-Store
-- Key: String
-- Wert: Integer

-- Wunschdenken:
{-
    put "Johannes" 36
    x = get "Johannes"
    put "Johannes" (x + 1)
    y = get "Johannes"
    return (show (x + y))
-}

-- data DBCommand a =
--     Put String Integer
--     | Get String
--     | Return a
--     deriving Show

-- >>> :t Get "johannes"
-- Get "johannes" :: DBCommand

-- DB-Programm ist Liste/Abfolge von Commands
-- type DBProgram = [DBCommand]

-- p1 = [ Put "Johannes" 36
--      , Get "Johannes" -- wo ist der Rückgabewert? bzw.
--      -- wie gebe ich ihm einen Namen?
--      ]

-- -> führt noch nicht zum Ziel

-- "Beschreibung eines Datenbankprogramms mit Ergebnis vom Typ a"
data DB a
    -- Idee: Callback, um Programm fortzusetzen
    -- Was ist, wenn wir nichts finden?
    = Get String (Integer -> DB a)
    --                    v hier könnte auch ID stehen
    | Put String Integer (() -> DB a)
    | Return a
    -- deriving Show

-- class IDB m where
--     get :: String -> m Integer
--     put :: String -> Integer -> m ()
--     return :: a -> m a

-- class ISendSms m where
--     send :: String -> PhoneNumber -> m ()

-- p1WithSms :: (IDB m, ISendSms m) => m String
-- p1WithSms = ... -- hier kann ich ISendSms und IDB gleichzeitig verwenden

p1 :: DB String
p1 = Put "Johannes" 36 (\ _ ->
     Get "Johannes" (\ x ->
     -- Nariman möchte hier SMS verschicken mit x
     -- TODO: algebraische Effekte
     -- - Monaden
     -- - MonadenTransformatoren
     -- - Freie Monaden
     -- - Tagless Final / mtl-Stil
     Put "Johannes" (x+1) (\ _ ->
     Get "Johannes" (\ y ->
     Return (show (x + y))))))

-- Wunsch: DB a -> a

-- Baustein, mit dem wir Ablauf zusammenbauen wollen
-- wie im Beispiel oben
-- -> _ist_ ein simples DB-Programm
get :: String -> DB Integer
get key = Get key Return -- (Return :: Integer -> DB Integer)

-- quasi void als Ergebnis
put :: String -> Integer -> DB ()
put key value = Put key value Return

return :: a -> DB a
return = Return

-- wir wollen den Ablauf bzw. das Programm _interpretieren_
runDB :: Map String Integer -> DB a -> (a, Map String Integer)
runDB mp (Get key callback) =
    let value = mp ! key 
        -- foo = "abc"
    in runDB mp (callback value) -- _ ist "typed hole"
runDB mp (Put key value callback) =
    let updatedMap = Map.insert key value mp
    in runDB updatedMap (callback ())
runDB mp (Return a) = (a, mp)

-- >>> runDB Map.empty p1
-- ("73",fromList [("Johannes",37)])

-- >>> runDB Map.empty (put "foo" 1)
-- ((),fromList [("foo",1)])

-- >>> runDB Map.empty (Return 3)
-- (3,fromList [])

--          v Typkonstruktor
-- class Monad' m where
--     return' :: a -> m a
--     -- bind / flatMap
--     (>>=) :: m a -> (a -> m b) -> m b

-- Datenbankprogramme verbinden
splice :: DB a -> (a -> DB b) -> DB b
splice (Return a) f = f a
splice (Get key callback) f =
    -- Alle Puzzleteile:
    -- callback :: Integer -> DB a
    -- f :: a -> DB b
    -- key :: String
    -- get key :: DB Integer
    -- splice :: DB a -> (a -> DB b) -> DB b
    
splice _ _ = undefined