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

data DBCommand =
    Put String Integer
    | Get String
    | Return String
    deriving Show

-- >>> :t Get "johannes"
-- Get "johannes" :: DBCommand

type DBProgram = [DBCommand]