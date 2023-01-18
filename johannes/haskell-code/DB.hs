module DB where

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

p1 :: DB String
p1 = Put "Johannes" 36 (\ _ ->
     Get "Johannes" (\ x ->
     -- Nariman möchte hier SMS verschicken mit x
     -- TODO: algebraische Effekte
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