module Library
    ( MetricPrefix (..)         -- (..) means exporting all data constructors
    , parseMetricPrefix
    , parseDouble
    ) where

import Data.Maybe
import Text.Parsec (oneOf, many1, digit, char, option, string', choice)
import Text.Parsec.String (Parser)
import Control.Applicative
import Control.Monad


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


parseMetricPrefix' :: String -> MetricPrefix
parseMetricPrefix' s
    | elem s ["a", "atto", "Atto"]   = Atto
    | elem s ["f", "femto", "Femto"] = Femto
    | elem s ["p", "pico", "Pico"]   = Pico
    | elem s ["n", "nano", "Nano"]   = Nano
    | elem s ["u", "μ", "mu", "Mu", "micro", "Micro"] = Micro
    | elem s ["m", "milli", "Milli"] = Milli
    | elem s ["K", "kilo", "Kilo"]   = Kilo
    | elem s ["M", "mega", "Mega"]   = Mega
    | elem s ["G", "giga", "Giga"]   = Giga
    | elem s ["T", "tera", "Tera"]   = Tera
    | elem s ["P", "peta", "Peta"]   = Peta
    | elem s ["E", "exa", "Exa"]     = Exa
    | otherwise = error $ "Unexpected Char \"" ++ s ++ "\" for MetricPrefix"


parseMetricPrefix :: Parser MetricPrefix
parseMetricPrefix = do
    str <-  choice $ fmap string' [         -- Note that we must use string' instead of string
        -- full form first
          "atto", "Atto"
        , "femto", "Femto"
        , "pico", "Pico"
        , "nano", "Nano"
        , "mu", "Mu", "micro", "Micro"
        , "milli", "Milli"
        , "kilo", "Kilo"
        , "mega", "Mega"
        , "giga", "Giga"
        , "tera", "Tera"
        , "peta", "Peta"
        , "exa", "Exa"
        -- then abbreviative
        , "a", "f", "p"
        , "n", "u", "μ"
        , "m", "K", "M"
        , "G", "T" , "P", "E"
        ]
    return $ parseMetricPrefix' str


data Units =
      ElectronVolt
    | KiloCaloriePerMole
    | KiloJoulePerMole
    | Kelvin
    | Hartree
    | Wavenumber
    | NanoMeter
    | TeraHertz
    deriving (Show)

--instance Show Units where
    --show ElectronVolt = "eV"
    --show KiloCaloriePerMole = "KCal/mol"
    --show KiloJoulePerMole = "KJ/mol"
    --show Kelvin = "K"
    --show Hartree = "har"
    --show Wavenumber = "cm-1"
    --show NanoMeter = "nm"
    --show TeraHertz = "THz"



-- Now we are going to parse the float point numbers
parseDigit :: Parser Char
parseDigit = digit


parseIntegral :: Parser String
parseIntegral = many1 parseDigit


parseSign :: Parser Char
parseSign = char '-' <|> char '+'


parseOptionalSign :: Parser String
parseOptionalSign = option "" $ fmap (:[]) parseSign


parseFractional :: Parser String
parseFractional = pure (:) <*> char '.' <*> parseIntegral


parseExponent :: Parser String
parseExponent = do
    e <- oneOf "eE"
    s <- parseOptionalSign
    i <- parseIntegral
    return $ (e:s) ++ i
--parseExponent  = pure (:) <*> oneOf "eE" <*> ints
    --where ints = pure (++) <*> parseOptionalSign <*> parseIntegral


parseDouble :: Parser Double
parseDouble = do
    s <- parseOptionalSign
    i <- parseIntegral
    f <- option "" parseFractional
    e <- option "" parseExponent
    let floatStr = s ++ i ++ f ++ e
    return (read floatStr :: Double)
