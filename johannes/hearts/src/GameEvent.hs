module GameEvent where

import Cards
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- Events: Log anlegen von Ereignissen, die in der Anwendung passieren
-- Log enthält alles, was passiert ist -> Erzählung

-- (kein) Event-Sourcing: Events als Quelle der Wahrheit

-- Events:
-- - liegen in der Vergangenheit
-- - fachlich motiviert/orientiert
-- - müssen *alles* erzählen
-- - Redundanz ist vollkommen OK
-- - keinen Zustand reinschreiben

-- Commands:
-- - Bitte, dass etwas passieren soll _in der Zukunft_

-- hier:

-- siehe Bild

data GameEvent
    -- = HandsDealt [(Player, Hand)]
    = HandDealt Player Hand
    | PlayerTurnChanged Player
    | LegalCardPlayed Player Card
    | TrickTaken Player Trick
    -- könnte auch zweiteilen in TrickClosed und TrickTaken
    | GameEnded Player
    | IllegalCardAttempted Player Card
    deriving Show

data GameCommand
    = PlayCard Player Card
    -- | TakeTrick Player
    | DealHands (Map Player Hand)
    deriving Show
    -- | Hat wer gewonnen?

-- was wissen wir über den Spielablauf?
-- Spielablauf ist "wie DB" -> Monade

-- ein einzelner Schritt
turnOverTrickM :: Game (Maybe (Player, Trick))
turnOverTrickM = TurnOverTrick Done

isPlayValidM :: Player -> Card -> Game Bool
isPlayValidM player card = PlayValid player card Done

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Done

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Done

waitForCommandM :: Game GameCommand
waitForCommandM = WaitForCommand Done

-- Game enthält die abstrakten Schritte des Spielablaufs, losgelöst 
-- vom jeglichem Zustand
data Game a =
    -- Done :: a -> Game a
    TurnOverTrick (Maybe (Player, Trick) -> Game a)
    | PlayValid Player Card (Bool -> Game a)
    | RecordEvent GameEvent (() -> Game a)
    | GameOver (Maybe Player -> Game a)
    | PlayerAfter Player (Player -> Game a)
    | WaitForCommand (GameCommand -> Game a)
    | Done a

instance Functor Game where
instance Applicative Game where

instance Monad Game where
    return = Done
    -- (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Done a) next = next a
    (>>=) (TurnOverTrick callback) next =
        -- callback :: Maybe (Player, Trick) -> Game a
        -- next :: a -> Game b
        -- (>>=)
        TurnOverTrick (\ x -> (callback x) >>= next)
    (>>=) (PlayValid player card callback) next =
        PlayValid player card (\ x -> (callback x) >>= next)
    (>>=) (GameOver callback) next =
        GameOver (\ x -> (callback x) >>= next)
    (>>=) (PlayerAfter player callback) next =
        PlayerAfter player (\ x -> (callback x) >>= next)
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\ x -> (callback x) >>= next)
    (>>=) (WaitForCommand callback) next =
        WaitForCommand (\ x -> (callback x) >>= next)

-- Tisch erhält einzelnes Command
-- -> gibt den nächsten Schritt zurück
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands hands) =
    mapM_ (\ h -> recordEventM (HandDealt h)) (Map.toList hands)
tableProcessCommand (PlayCard player card) = do
    isValid <- isPlayValidM player card
    if isValid 
        then do
            recordEventM (LegalCardPlayed player card)
            turnOverTrick <- turnOverTrickM
            case turnOverTrick of
                -- jemand bekommt den Stich
                Just (trickTaker, trick) -> do
                    recordEventM (TrickTaken trickTaker trick)
                    potentialWinner <- gameOverM
                    case potentialWinner of
                        Just winner -> do
                            recordEventM (GameEnded winner)
                            return (Just winner)
                        Nothing -> do
                            recordEventM (PlayerTurnChanged trickTaker)
                            return Nothing
                Nothing -> do
                    nextPlayer <- playerAfterM player
                    recordEventM (PlayerTurnChanged nextPlayer)
                    return Nothing
        else do
            recordEventM (IllegalCardAttempted player card)
            return Nothing

-- das gesamte Spiel
tableLoopM :: GameCommand -> Game Player
tableLoopM command = do
    maybeWinner <- tableProcessCommand command
    case maybeWinner of
        Just winner ->
            return winner
        Nothing -> do
            nextCommand <- waitForCommandM
            tableLoopM nextCommand
            -- GetCommand tableLoopM

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest
