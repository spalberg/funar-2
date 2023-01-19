{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards
import Data.Map

data GameEvent
  = HandDealt (Player, Hand)
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | GameEnded Player
  | IllegalCardAttempted Player Card

data GameCommand
  = PlayCard Player Card
  | DealHands (Map Player Hand)

-- Was wissen wir über den Spielablauf

data Game a =
  Done a

instance Monad Game where
  return :: a -> Game a
  return = Done
  (>>=) :: Game a -> (a -> Game b) -> Game b
  (>>=) = undefined
