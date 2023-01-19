{-# LANGUAGE InstanceSigs #-}
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

p1' :: DB Integer
p1' = Put "Johannes" 36 (\ _ ->
     Get "Johannes" (\ x ->
     -- Nariman möchte hier SMS verschicken mit x
     -- TODO: algebraische Effekte
     -- - Monaden
     -- - MonadenTransformatoren
     -- - Freie Monaden
     -- - Tagless Final / mtl-Stil
     Put "Johannes" (x+1) (\ _ ->
     Get "Johannes" (\ y ->
     Return (x + y)))))

-- Idee: show oben rausziehen
p1'AsString :: DB String
p1'AsString = fmap show p1'

-- Wunsch: DB a -> a

-- Baustein, mit dem wir Ablauf zusammenbauen wollen
-- wie im Beispiel oben
-- -> _ist_ ein simples DB-Programm
get :: String -> DB Integer
get key = Get key Return -- (Return :: Integer -> DB Integer)

-- quasi void als Ergebnis
put :: String -> Integer -> DB ()
put key value = Put key value Return

-- return :: a -> DB a
-- return = Return

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
splice (Get key callback) f =
    -- Alle Puzzleteile:
    -- callback :: Integer -> DB a
    -- f :: a -> DB b
    -- key :: String
    -- get key :: DB Integer
    -- splice :: DB a -> (a -> DB b) -> DB b
    -- Get :: String -> (Integer -> DB a)
    -- Put ...
    -- Return ...

    -- brauchen hier: _ein konkretes_ DB b
    -- Michael: woher kriegt man den Integer?
    Get key (\ i ->
        splice (callback i) f)
splice (Put key value callback) f =
    Put key value (\ _ ->
        splice (callback ()) f)
-- Return wird ersetzt durch das NEUE Restprogramm, aka f a
splice (Return a) f = f a


-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b

-- Übung: Funktorinstanz für DB

instance Functor DB where
    -- fmap :: (a -> b) -> DB a -> DB b
    fmap f db = case db of
      Get key callback ->
        Get key (\ i -> fmap f (callback i))
      Put key value callback ->
        Put key value (\ _ -> fmap f (callback ()))
      Return a -> Return (f a)

-- Dummy-Implementierung
instance Applicative DB where

instance Monad DB where
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice
    return :: a -> DB a
    return = Return

p1'' :: DB String
-- p1'' = Put "Johannes" 36 (\ _ ->
--      Get "Johannes" (\ x ->
--      Put "Johannes" (x+1) (\ _ ->
--      Get "Johannes" (\ y ->
--      Return (show (x + y))))))
p1'' = splice (put "Johannes" 36) (\ () ->
       splice (get "Johannes") (\ x ->
       splice (put "Johannes" (x+1)) (\ _ ->
       splice (get "Johannes") (\ y ->
       Return (show (x + y))))))

p1''alternativ :: DB String
p1''alternativ =
    put "Johannes" 36 >>= (\ () ->
    get "Johannes" >>= (\ x ->
    put "Johannes" (x+1) >>= (\ _ ->
    get "Johannes" >>= (\ y ->
    Return (show (x + y))))))

-- monadische Syntax / do-Notation
p1''' :: DB String
p1''' = do
  put "Johannes" 36
  x <- get "Johannes"
  foo <- p1''alternativ
  put "Johannes" (x + 1)
  y <- get "Johannes"
  return (show (x+y))

-- Hello world
hello :: IO ()
hello = do
    putStrLn "hello world"
    putStrLn "foobar"
    s <- getLine
    putStrLn ("du sagtest: " <> s)

data Optional a =
    Result a
    | Null
    deriving (Eq, Show)

instance Functor Optional where
    fmap :: (a -> b) -> Optional a -> Optional b
    fmap f Null = Null
    fmap f (Result a) = Result (f a)

instance Applicative Optional where

instance Monad Optional where
    return :: a -> Optional a
    return = Result

    (>>=) :: Optional a -> (a -> Optional b) -> Optional b
    (>>=) Null _ = Null
    (>>=) (Result a) f = f a
    -- (Result a) >>= f = undefined

-- >>> Result "abc" >>= (\ s -> Result (s ++ "foo"))
-- Result "abcfoo"

-- >>> Null >>= (\ s -> Result (s ++ "foo"))
-- Null

-- optProg :: Optional String
-- optProg = do
--     let xs = ["a", "b", "c"]
--     d <- listIndex 5 xs
--     a <- listIndex 0 xs
--     b <- listIndex 1 xs
--     return (a ++ b ++ d)

optProg :: Optional String
optProg = do
    d <- Result "foo"
    a <- Null
    b <- Result "bar"
    return (a ++ b ++ d)

-- >>> optProg
-- Null

-- ADT:

-- GADTs:
-- data Feature a where
--     FloatInputVariable :: String -> Feature Float
--     StringInputVariable :: String -> Feature String
--     Normalize :: Float -> Feature Float -> Feature Float
-- sonst: Validierung