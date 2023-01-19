module Contract where

{-

1. einfaches Beispiel:

Ich bekomme am 24.12.2023 100 Euro.
Zero-Coupon-Bond

2. geht's vielleicht noch einfacher?
   Beispiel zerlegen:

   -- Währung 
   -- Betrag / Anzahl
   -- Datum

   oder mit Default:

   -- Ich bekomme 1 Euro jetzt.

Weitere Beispiele:
-- Currency swap:
  Am 24.12.2023:
  - ich bekomme 100 EUR (long-Position)
  - ich bezahle 150 GBP (short-Position)

-}

data Date = MkDate String
    deriving Show

data Currency = EUR | GBP | USD | YEN
    deriving Show

type Amount = Float

-- data Contract 
--     -- = ZeroCouponBond Date Currency Amount
--     -- | CurrencySwap Date (Currency, Amount) (Currency, Amount)
--     = ZahlungAm Date Currency Amount
--     -- | Future Date ...

-- data CurrencySwap = MkCurrencySwap Contract Contract

data Contract
    -- Nariman: Amount ist "überführt in etwas Vergleichbares"
    -- = MkAmount Amount Currency
    = One Currency
    | Times Amount Contract -- <- Selbstbezug
    | Plus Contract Contract
    | AtDate Date Contract
    | OneOf Contract Contract
    | Max Contract Contract
    | Min Contract Contract
    | Short Contract
    deriving Show

-- >>> One EUR
-- One EUR


-- >>> Times 100 (One EUR)
-- Times 100.0 (One EUR)

-- Jetzt: ZCB
-- >>> AtDate (MkDate "24.12.2023") (Times 100 (One EUR))
-- AtDate (MkDate "24.12.2023") (Times 100.0 (One EUR))

oneEuro :: Contract
oneEuro = One EUR

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    AtDate date (Times amount (One currency))

-- Currency swap:
-- >>> Plus (Short (zeroCouponBond (MkDate "24.12.2023") 150 GBP)) (zeroCouponBond (MkDate "24.12.2023") 100 EUR)
-- Plus (Short (AtDate (MkDate "24.12.2023") (Times 150.0 (One GBP)))) (AtDate (MkDate "24.12.2023") (Times 100.0 (One EUR)))

-- alternativ:
-- >>> AtDate (MkDate "24.12.2023") (Plus (Short (Times 150 (One GBP))) (Times 100 oneEuro))
-- AtDate (MkDate "24.12.2023") (Plus (Short (Times 150.0 (One GBP))) (Times 100.0 (One EUR)))

-- haben: Syntax
-- wollen: Semantik

-- welche Zahlungen sind vonnöten an Datum xyz bei Contract c?

data Direction = MkShort | MkLong

data Payment = MkPayment Direction Date Amount Currency

-- Frage: macht man (, Contract) gleich?
-- wie kommt man darauf?
-- State-Monade:

-- data State s a = MkState (s -> (s, a))

semantics :: Contract -> Date -> ([Payment], Contract)
semantics = undefined