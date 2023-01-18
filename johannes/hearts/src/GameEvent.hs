module GameEvent where

import Cards
import Data.Map.Strict (Map)

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

data GameCommand
    = PlayCard Player Card
    -- | TakeTrick Player
    | DealHands (Map Player Hand)
    -- | Hat wer gewonnen?