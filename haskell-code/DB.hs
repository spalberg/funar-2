{-# LANGUAGE InstanceSigs #-}

module DB where

-- <- Alias
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Datenbank: Key-Value-Store
-- Key: String
-- Wert: Integer

-- Wunschdenken
{-
    put "Sven" 27
    x = get "Sven"
    put "Sven" (x + 1)
    y = get "Sven"
    return (show (x + y))
-}

-- data DBCommand a
--   = Put String Integer
--   | Get String
--   | Return a
--   deriving (Show)

-- type DBProgramm = [DBCommand]

--
data DB a
  = Get String (Integer -> DB a)
  | Put String Integer (() -> DB a)
  | Return a

p1 :: DB String
p1 =
  Put
    "Sven"
    27
    ( \() ->
        Get
          "Sven"
          ( \x ->
              Put
                "Sven"
                (x + 1)
                ( \() ->
                    Get
                      "Sven"
                      ( \y ->
                          Return (show (x + y))
                      )
                )
          )
    )

get :: String -> DB Integer
get key = Get key Return

put :: String -> Integer -> DB ()
put key value = Put key value Return

-- return :: a -> DB a
-- return = Return

-- >>> get "Sven"
-- No instance for (Show (DB Integer))
--   arising from a use of ‘evalPrint’

runDB :: Map String Integer -> DB a -> (a, Map String Integer)
runDB mp (Get key cb) =
  let value = mp ! key
   in runDB mp (cb value)
runDB mp (Put key value cb) =
  let mp' = Map.insert key value mp
   in runDB mp' (cb ())
runDB mp (Return a) = (a, mp)

-- >>> runDB Map.empty p1
-- ("55",fromList [("Sven",28)])

splice :: DB a -> (a -> DB b) -> DB b
splice (Return a) f = f a
splice (Get key cb) f =
  Get key (\i -> splice (cb i) f)
splice (Put key value cb) f =
  Put key value (\_ -> splice (cb ()) f)

instance Functor DB where
  fmap :: (a -> b) -> DB a -> DB b
  fmap f (Return a) = Return (f a)
  fmap f (Get key cb) = Get key (\i -> fmap f (cb i))
  fmap f (Put key value cb) = Put key value (\_ -> fmap f (cb ()))

instance Applicative DB

instance Monad DB where
  return :: a -> DB a
  return = Return
  (>>=) :: DB a -> (a -> DB b) -> DB b
  (>>=) = splice

p1' :: DB String
p1' =
  splice
    (put "Sven" 27)
    ( \_ ->
        splice
          (get "Sven")
          ( \x ->
              splice
                (put "Sven" (x + 1))
                ( \_ ->
                    splice
                      (get "Sven")
                      ( \y ->
                          Return (show (x + y))
                      )
                )
          )
    )

p1'' :: DB String
p1'' = do
  put "Sven" 27
  x <- get "Sven"
  put "Sven" (x + 1)
  y <- get "Sven"
  return (show (x + y))

-- >>> runDB Map.empty p1''
-- ("55",fromList [("Sven",28)])

hello :: IO ()
hello = do
  putStrLn "hello world"
  s <- getLine
  putStrLn ("du sagtest: " <> s)
