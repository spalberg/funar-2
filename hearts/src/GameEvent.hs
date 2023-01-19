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
