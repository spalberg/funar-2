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


data DB a
    -- Idee: Callback, um Programm fortzusetzen
    -- Was ist, wenn wir nichts finden?
    = Get String (Integer -> b)