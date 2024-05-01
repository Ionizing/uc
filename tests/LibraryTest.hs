module Main where

import Library (
    MetricPrefix (..)                       -- (..) means importing all data constructors
  , parseMetricPrefix
  , Unit (..)
  , parseUnit
  , parseDouble
  , Quantity (..)
  , parseQuantity
  )
import Test.HUnit
import qualified System.Exit as Exit
import Text.Parsec.String
import Text.Parsec


testAssertSingle :: (Eq a, Show a) => a -> (Parser a) -> String -> Test
testAssertSingle expect pfun instr = TestCase $ let
    msg = "\"" ++ instr ++ "\" should be parsed as \"" ++ show expect ++ "\""
    res = case (parse pfun msg instr) of
        Right a -> a
        Left  a -> error $ "test failed: " ++ msg ++ ", while the actual result is " ++ show a
    in res @?= expect


testParseMetricPrefix :: Test
testParseMetricPrefix = TestList [
      TestList [testAssertSingle Atto   parseMetricPrefix str | str <- ["a", "atto", "Atto"]]
    , TestList [testAssertSingle Femto  parseMetricPrefix str | str <- ["f", "femto", "Femto"]]
    , TestList [testAssertSingle Pico   parseMetricPrefix str | str <- ["p", "pico", "Pico"]]
    , TestList [testAssertSingle Nano   parseMetricPrefix str | str <- ["n", "nano", "Nano"]]
    , TestList [testAssertSingle Micro  parseMetricPrefix str | str <- ["u", "μ", "mu", "Mu", "micro", "Micro"]]
    , TestList [testAssertSingle Milli  parseMetricPrefix str | str <- ["m", "milli", "Milli"]]
    , TestList [testAssertSingle Kilo   parseMetricPrefix str | str <- ["K", "Kilo", "kilo"]]
    , TestList [testAssertSingle Mega   parseMetricPrefix str | str <- ["M", "mega", "Mega"]]
    , TestList [testAssertSingle Giga   parseMetricPrefix str | str <- ["G", "giga", "Giga"]]
    , TestList [testAssertSingle Tera   parseMetricPrefix str | str <- ["T", "tera", "Tera"]]
    , TestList [testAssertSingle Peta   parseMetricPrefix str | str <- ["P", "peta", "Peta"]]
    , TestList [testAssertSingle Exa    parseMetricPrefix str | str <- ["E", "exa", "Exa"]]
    ]


testParseUnit :: Test
testParseUnit = TestList [
      TestList [testAssertSingle ElectronVolt   parseUnit str | str <- ["eV", "ElectronVolt"]]
    , TestList [testAssertSingle CaloriePerMole parseUnit str | str <- ["Ca/mol", "Calorie/mol", "Ca", "Calorie"]]
    , TestList [testAssertSingle JoulePerMole   parseUnit str | str <- ["J/mol", "Joule/mol", "J", "Joule"]]
    , TestList [testAssertSingle Kelvin         parseUnit str | str <- ["K", "Kelvin"]]
    , TestList [testAssertSingle Wavenumber     parseUnit str | str <- ["cm-1", "Cm-1"]]
    , TestList [testAssertSingle Meter          parseUnit str | str <- ["m", "Meter"]]
    , TestList [testAssertSingle Hertz          parseUnit str | str <- ["Hz", "Hertz"]]
    ]


testParseDouble :: Test
testParseDouble = TestList [
      TestList [testAssertSingle 114.514 parseDouble str | str <- ["114.514", "114.514E0", "114.514E"]]
    , testAssertSingle 114514.0 parseDouble "114.514E3"
    ]


testParseQuantity :: Test
testParseQuantity = TestList [
      TestList [testAssertSingle Quantity { number=114.514, prefix=None, unit=ElectronVolt } parseQuantity str 
            | str <- ["114.514 eV", "114.514eV", "114.514 ElectronVolt", "114.514ElectronVolt"]]
    , TestList [testAssertSingle Quantity { number=114.514, prefix=Exa, unit=ElectronVolt } parseQuantity str 
            | str <- [ "114.514 EeV", "114.514EeV" , "114.514 E eV", "114.514E eV"
                     , "114.514EElectronVolt","114.514 EElectronVolt"
                     , "114.514E ElectronVolt","114.514 E ElectronVolt"
                     , "1.14514E2EeV"
                     ]]
    , TestList [testAssertSingle Quantity { number=114.514, prefix=Kilo, unit=ElectronVolt } parseQuantity str 
            | str <- [ "114.514 KeV", "114.514KeV", "114.514KElectronVolt", "114.514 KElectronVolt"
                     , "114.514 K eV", "114.514K eV", "114.514 K ElectronVolt", "114.514K ElectronVolt"
                     ]]
    , TestList [testAssertSingle Quantity { number=114.514, prefix=None, unit=Meter } parseQuantity str 
            | str <- ["114.514 m", "114.514m", "114.514 Meter", "114.514Meter"]]
    , TestList [testAssertSingle Quantity { number=114.514, prefix=Milli, unit=Meter } parseQuantity str 
            | str <- [ "114.514 Milli Meter", "114.514 MilliMeter", "114.514MilliMeter", "114.514Milli Meter"
                     , "114.514mm", "114.514 mm", "114.514m m", "114.514 m m"
                     ]]
    , TestList [testAssertSingle Quantity { number=114.514, prefix=None, unit=Kelvin } parseQuantity str 
            | str <- ["114.514 K", "114.514K", "114.514 Kelvin", "114.514Kelvin"]]
    , TestList [testAssertSingle Quantity { number=114.514, prefix=Kilo, unit=Kelvin } parseQuantity str 
            | str <- ["114.514 K K", "114.514 KK", "114.514K K", "114.514KK"]]
    ]


main :: IO Counts
main = do
    runTestTT testParseMetricPrefix
    runTestTT testParseUnit
    runTestTT testParseDouble
    runTestTT testParseQuantity
