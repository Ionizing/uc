module Library
    ( MetricPrefix (..)         -- (..) means exporting all data constructors
    , parseMetricPrefix
    ) where


data MetricPrefix =
      Atto        -- 10^-18
    | Femto       -- 10^-15
    | Pico        -- 10^-12
    | Nano        -- 10^-9
    | Micro       -- 10^-6
    | Milli       -- 10^-3
    | None        -- 1
    | Kilo        -- 10^3
    | Mega        -- 10^6
    | Giga        -- 10^9
    | Tera        -- 10^12
    | Peta        -- 10^15
    | Exa         -- 10^18
    deriving (Show, Eq, Ord)


parseMetricPrefix :: Char -> Maybe MetricPrefix
parseMetricPrefix ch = case ch of
    'a' -> Just Atto
    'f' -> Just Femto
    'p' -> Just Pico
    'n' -> Just Nano
    'u' -> Just Micro
    'm' -> Just Milli
    'K' -> Just Kilo
    'k' -> Just Kilo
    'M' -> Just Mega
    'G' -> Just Giga
    'g' -> Just Giga
    'T' -> Just Tera
    't' -> Just Tera
    'P' -> Just Peta
    'E' -> Just Exa
    _   -> Nothing
