module Library
    ( MetricPrefix (..)         -- (..) means exporting all data constructors
    , parseMetricPrefix
    , Unit (..)
    , parseUnit
    , parseDouble
    , Quantity (..)
    , parseQuantity
    ) where

import Data.Maybe
import Text.Parsec (oneOf, many1, digit, char, option, string', choice, spaces, try, eof)
import Text.Parsec.String (Parser)
import Control.Applicative
import Control.Monad


-- Metrix prefix parser
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
    | otherwise = error $ "Unexpected String \"" ++ s ++ "\" for MetricPrefix"


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


-- Unit parser
data Unit =
      ElectronVolt      -- eV
    | CaloriePerMole    -- Ca/mol
    | JoulePerMole      -- J/mol
    | Kelvin            -- K
    | Hartree           -- Ha
    | Wavenumber        -- cm-1
    | Meter             -- m
    | Hertz             -- Hz
    deriving (Show, Eq)


parseUnit' :: String -> Unit
parseUnit' s
    | elem s ["eV", "ElectronVolt"] = ElectronVolt
    | elem s ["Ca/mol", "Calorie/mol", "Ca", "Calorie"] = CaloriePerMole
    | elem s ["J/mol", "Joule/mol", "J", "Joule"] = JoulePerMole
    | elem s ["K", "Kelvin"] = Kelvin
    | elem s ["Ha", "Hartree"] = Hartree
    | elem s ["cm-1", "Cm-1"] = Wavenumber
    | elem s ["m", "Meter"] = Meter
    | elem s ["Hz", "Hertz"] = Hertz
    | otherwise = error $ "Unexpected String \"" ++ s ++ "\" for Unit"


parseUnit :: Parser Unit
parseUnit = do
    str <- choice $ fmap string' [
          "ElectronVolt"
        , "Ca/mol", "Calorie/mol", "Calorie"
        , "J/mol", "Joule/mol", "Joule"
        , "Kelvin"
        , "Hartree", "Hartree"
        , "Cm-1", "cm-1"
        , "Meter"
        , "Hertz"
        , "eV", "Ca", "J", "K", "m", "Hz"
        ]
    return $ parseUnit' str


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


parseDouble :: Parser Double
parseDouble = do
    s <- parseOptionalSign
    i <- parseIntegral
    f <- option "" parseFractional
    e <- try parseExponent <|> return ""
    let floatStr = s ++ i ++ f ++ e
    return (read floatStr :: Double)


-- Define quantity as Number ++ spaces? ++ MetricPrefix? ++ spaces? ++ Unit
data Quantity = Quantity {
      number :: Double
    , prefix :: MetricPrefix
    , unit   :: Unit
    } deriving (Eq, Show)


parseQuantityNoPrefix :: Parser Quantity
parseQuantityNoPrefix = do
    number <- parseDouble
    spaces
    unit <- parseUnit
    eof
    return Quantity {number = number, prefix = None, unit = unit}


parseQuantityWithPrefix :: Parser Quantity
parseQuantityWithPrefix = do
    number <- parseDouble
    spaces
    prefix <- option None parseMetricPrefix
    spaces
    unit <- parseUnit
    eof
    return Quantity {number = number, prefix = prefix, unit = unit}


parseQuantity :: Parser Quantity
parseQuantity = try parseQuantityNoPrefix <|> try parseQuantityWithPrefix


-- eliminate metricprefix, and convert all the energy to JoulePerMole
normalizePrefix :: Quantity -> Quantity
normalizePrefix Quantity {number=number, prefix=prefix, unit=unit} = Quantity {number=number*scale, prefix=None, unit=unit}
    where
    scale = case prefix of
        Atto  -> 1E-18
        Femto -> 1E-15
        Pico  -> 1E-12
        Nano  -> 1E-9
        Micro -> 1E-6
        Milli -> 1E-3
        None  -> 1.0
        Kilo  -> 1E3
        Mega  -> 1E6
        Giga  -> 1E9
        Tera  -> 1E12
        Peta  -> 1E15
        Exa   -> 1E18


-- conversion ratios
eVToEv :: Double
eVToEv = 1.0

eVToJoulePerMole :: Double
eVToJoulePerMole = 1.60217733 * 6.0223 * 1E4

eVToCaloriePerMole :: Double
eVToCaloriePerMole = eVToJoulePerMole / 4184

eVToKelvin :: Double
eVToKelvin = 1.160451812E4

eVToHartree :: Double
eVToHartree = 1.0 / 27.2114

eVToWavenumber :: Double
eVToWavenumber = 8065.73

eVToMeter :: Double
eVToMeter = 1.23984193E-6

eVToHertz :: Double
eVToHertz = 2.417989242E14


normalizeUnit :: Quantity -> Quantity
normalizeUnit Quantity {number=number, prefix=None, unit=unit} = Quantity {number=newNumber, prefix=None, unit=ElectronVolt}
    where
    newNumber = case unit of
        ElectronVolt   -> number * eVToEv
        CaloriePerMole -> number * eVToCaloriePerMole
        JoulePerMole   -> number * eVToJoulePerMole
        Kelvin         -> number * eVToKelvin
        Hartree        -> number * eVToHartree
        Wavenumber     -> number * eVToWavenumber
        Meter          -> eVToMeter / number
        Hertz          -> number * eVToHertz

normalizeUnit _ = error "You should eliminate the MetricPrefix first (via normalizePrefix)"


normalizeQuantity :: Quantity -> Quantity
normalizeQuantity = normalizePrefix . normalizeUnit
