module Library
    ( MetricPrefix (..)         -- (..) means exporting all data constructors
    , parseMetricPrefix
    , Unit (..)
    , parseUnit
    , parseDouble
    , Quantity (..)
    , parseQuantity
    , convertQuantity
    ) where

import Data.Map.Strict (Map, keys, fromList, fromAscList, (!))
import Text.Printf
import Text.Parsec (oneOf, many1, digit, char, option, string', choice, spaces, try, eof)
import Text.Parsec.String (Parser)
import Control.Applicative


-- Metric prefix parser
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


prefixScale :: Map MetricPrefix Double
prefixScale = fromAscList [
      (Atto,  1E-18)
    , (Femto, 1E-15)
    , (Atto,  1E-18)
    , (Femto, 1E-15)
    , (Pico,  1E-12)
    , (Nano,  1E-9)
    , (Micro, 1E-6)
    , (Milli, 1E-3)
    , (None,  1.0)
    , (Kilo,  1E3)
    , (Mega,  1E6)
    , (Giga,  1E9)
    , (Tera,  1E12)
    , (Peta,  1E15)
    , (Exa,   1E18)
    ]


prefixAbbreviative :: Map MetricPrefix String
prefixAbbreviative = fromList [
      (Atto,  "a")
    , (Femto, "f")
    , (Pico,  "p")
    , (Nano,  "n")
    , (Micro, "μ")
    , (Milli, "m")
    , (Kilo,  "K")
    , (Mega,  "M")
    , (Giga,  "G")
    , (Tera,  "T")
    , (Peta,  "P")
    , (Exa,   "E")
    ]


instance PrintfArg MetricPrefix where
    formatArg x fmt |
        fmtChar (vFmt 'P' fmt) == 'P' =
            formatString (show x) (fmt {fmtChar='s', fmtPrecision = Nothing})
    formatArg x fmt |
        fmtChar (vFmt 'p' fmt) == 'p' =
            formatString str (fmt {fmtChar='s', fmtPrecision = Nothing})
            where
            str = prefixAbbreviative ! x
    formatArg _ fmt = errorBadFormat $ fmtChar fmt


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
    | CaloriePerMole    -- Cal/mol
    | JoulePerMole      -- J/mol
    | Kelvin            -- K
    | Hartree           -- Ha
    | Wavenumber        -- cm-1
    | Meter             -- m
    | Hertz             -- Hz
    deriving (Show, Eq, Ord)


unitAbbreviate :: Map Unit String
unitAbbreviate = fromList [
      (ElectronVolt,    "eV")
    , (CaloriePerMole,  "Cal/mol")
    , (JoulePerMole,    "J/mol")
    , (Kelvin,          "K")
    , (Hartree,         "Ha")
    , (Wavenumber,      "cm-1")
    , (Meter,           "m")
    , (Hertz,           "Hz")
    ]


unitFullForm :: Map Unit String
unitFullForm = fromList [
      (ElectronVolt,    "ElectronVolt")
    , (CaloriePerMole,  "Calorie/mol")
    , (JoulePerMole,    "Joule/mol")
    , (Kelvin,          "Kelvin")
    , (Hartree,         "Hartree")
    , (Wavenumber,      "Cm-1")
    , (Meter,           "Meter")
    , (Hertz,           "Hertz")
    ]


instance PrintfArg Unit where
    formatArg x fmt |
        fmtChar (vFmt 'U' fmt) == 'U' =
            formatString str (fmt {fmtChar='s', fmtPrecision = Nothing})
            where
            str = unitFullForm ! x
    formatArg x fmt |
        fmtChar (vFmt 'u' fmt) == 'u' =
            formatString str (fmt {fmtChar='s', fmtPrecision = Nothing})
            where
            str = unitAbbreviate ! x
    formatArg _ fmt = errorBadFormat $ fmtChar fmt


parseUnit' :: String -> Unit
parseUnit' s
    | elem s ["eV", "ElectronVolt"] = ElectronVolt
    | elem s ["Cal/mol", "Calorie/mol", "Cal", "Calorie"] = CaloriePerMole
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
        , "Cal/mol", "Calorie/mol", "Calorie"
        , "J/mol", "Joule/mol", "Joule"
        , "Kelvin"
        , "Hartree", "Hartree"
        , "Cm-1", "cm-1"
        , "Meter"
        , "Hertz"
        , "eV", "Cal", "J", "K", "m", "Hz"
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
    where scale = prefixScale ! prefix


-- conversion ratios
convertRatioEvToOther :: Map Unit Double
convertRatioEvToOther = fromAscList [
      (ElectronVolt,   1.0)
    , (CaloriePerMole, 1.60217733 * 6.0223 * 1E4 / 4184)
    , (JoulePerMole,   1.60217733 * 6.0223 * 1E4)
    , (Kelvin,         1.160451812E4)
    , (Hartree,        1.0 / 27.2114)
    , (Wavenumber,     8065.73)
    , (Meter,          1.23984193E-6)
    , (Hertz,          2.417989242E14)
    ]


convertRatio :: Map (Unit, Unit) Double     -- Unit -> Unit -> Double
convertRatio = fromList [
      ((u1, u2), c)
    | u1 <- keys convertRatioEvToOther
    , u2 <- keys convertRatioEvToOther
    , let c = convertRatioEvToOther!u2 / convertRatioEvToOther!u1
    ]


normalizeUnit :: Quantity -> Quantity
normalizeUnit Quantity {number=number, prefix=None, unit=unit} =
    Quantity {number=newNumber, prefix=None, unit=ElectronVolt}
    where
    newNumber = case unit of
        Meter -> (convertRatioEvToOther ! Meter) / number
        x | elem x [
              ElectronVolt
            , CaloriePerMole
            , JoulePerMole
            , Kelvin
            , Hartree
            , Wavenumber
            , Hertz
            ] -> number / (convertRatioEvToOther ! x)
normalizeUnit _ = error "You should eliminate the MetricPrefix first (via normalizePrefix)"


normalizeQuantity :: Quantity -> Quantity
normalizeQuantity = normalizeUnit . normalizePrefix
-- Now the Quantity should be Quantity {number=number, prefix=None, unit=ElectronVolt}


quantityFromElectronVolt :: Unit -> Quantity -> Quantity
quantityFromElectronVolt newUnit (Quantity {number=number, prefix=None, unit=ElectronVolt}) =
    Quantity {number=newNumber, prefix=None, unit=newUnit}
    where
    newNumber = case newUnit of
        Meter -> (convertRatioEvToOther ! Meter) / number
        x | elem x [
              ElectronVolt
            , CaloriePerMole
            , JoulePerMole
            , Kelvin
            , Hartree
            , Wavenumber
            , Hertz
            ] -> number * (convertRatioEvToOther ! x)

quantityFromElectronVolt _ Quantity {number=_, prefix=_, unit=_} =
    error "You should normalize this quantity first (via normalizeQuantity)"


addMetricPrefix :: Quantity -> Quantity
addMetricPrefix Quantity {number=number, prefix=None, unit=unit} =
    Quantity {number=newNumber, prefix=newPrefix, unit=unit}
    where
    (newNumber, newPrefix) = case abs number of
        x | x < 1E-15 -> (number / 1E-18, Atto)
        x | x < 1E-12 -> (number / 1E-15, Femto)
        x | x < 1E-9  -> (number / 1E-12, Pico)
        x | x < 1E-6  -> (number / 1E-9,  Nano)
        x | x < 1E-3  -> (number / 1E-6,  Micro)
        x | x < 1     -> (number / 1E-3,  Milli)
        x | x < 1E3   -> (number * 1   ,  None)
        x | x < 1E6   -> (number * 1E-3,  Kilo)
        x | x < 1E9   -> (number * 1E-6,  Mega)
        x | x < 1E12  -> (number * 1E-9,  Giga)
        x | x < 1E15  -> (number * 1E-12, Tera)
        x | x < 1E18  -> (number * 1E-15, Peta)
        _             -> (number * 1E-18,  Exa)
addMetricPrefix Quantity {number=_, prefix=prefix, unit=_} =
    error "You should eliminate the MetricPrefix first (via normalizePrefix)"


convertQuantity :: Unit -> Quantity -> Quantity
convertQuantity newUnit =
    addMetricPrefix . (quantityFromElectronVolt newUnit) . normalizeQuantity
