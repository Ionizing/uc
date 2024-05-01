module Main where

import Library (
    MetricPrefix (..)                       -- (..) means importing all data constructors
  , parseMetricPrefix
  , Unit (..)
  , parseUnit
  , Quantity (..)
  , parseQuantity
  )
import Test.HUnit
import qualified System.Exit as Exit
import Text.Parsec.String
import Text.Parsec


testAssertSingle :: (Eq a, Show a) => a -> (Parser a) -> String -> Test
testAssertSingle expect pfun instr = TestCase $ let
    msg = "\"" ++ show instr ++ "\" should be parsed as \"" ++ show expect ++ "\""
    res = case (parse pfun msg instr) of
        Right a -> a
        Left  a -> error $ "test failed: " ++ msg ++ ", while the actual result is " ++ show a
    in res @?= expect


testParseMetricPrefix :: Test
testParseMetricPrefix = TestList [
      testAssertSingle Atto parseMetricPrefix "a"
    , testAssertSingle Atto parseMetricPrefix "atto"
    , testAssertSingle Atto parseMetricPrefix "Atto"
    , testAssertSingle Femto parseMetricPrefix "f"
    , testAssertSingle Femto parseMetricPrefix "femto"
    , testAssertSingle Femto parseMetricPrefix "Femto"
    , testAssertSingle Pico parseMetricPrefix "p"
    , testAssertSingle Pico parseMetricPrefix "pico"
    , testAssertSingle Pico parseMetricPrefix "Pico"
    , testAssertSingle Nano parseMetricPrefix "n"
    , testAssertSingle Nano parseMetricPrefix "nano"
    , testAssertSingle Nano parseMetricPrefix "Nano"
    , testAssertSingle Micro parseMetricPrefix "u"
    , testAssertSingle Micro parseMetricPrefix "μ"
    , testAssertSingle Micro parseMetricPrefix "mu"
    , testAssertSingle Micro parseMetricPrefix "Mu"
    , testAssertSingle Micro parseMetricPrefix "micro"
    , testAssertSingle Micro parseMetricPrefix "Micro"
    , testAssertSingle Milli parseMetricPrefix "m"
    , testAssertSingle Milli parseMetricPrefix "milli"
    , testAssertSingle Milli parseMetricPrefix "Milli"
    , testAssertSingle Kilo parseMetricPrefix "K"
    , testAssertSingle Kilo parseMetricPrefix "kilo"
    , testAssertSingle Kilo parseMetricPrefix "Kilo"
    , testAssertSingle Mega parseMetricPrefix "M"
    , testAssertSingle Mega parseMetricPrefix "mega"
    , testAssertSingle Mega parseMetricPrefix "Mega"
    , testAssertSingle Giga parseMetricPrefix "G"
    , testAssertSingle Giga parseMetricPrefix "giga"
    , testAssertSingle Giga parseMetricPrefix "Giga"
    , testAssertSingle Tera parseMetricPrefix "T"
    , testAssertSingle Tera parseMetricPrefix "tera"
    , testAssertSingle Tera parseMetricPrefix "Tera"
    , testAssertSingle Peta parseMetricPrefix "P"
    , testAssertSingle Peta parseMetricPrefix "peta"
    , testAssertSingle Peta parseMetricPrefix "Peta"
    , testAssertSingle Exa parseMetricPrefix "E"
    , testAssertSingle Exa parseMetricPrefix "exa"
    , testAssertSingle Exa parseMetricPrefix "Exa"
    ]


testParseUnit :: Test
testParseUnit = TestList [
      testAssertSingle ElectronVolt parseUnit "eV"
    , testAssertSingle ElectronVolt parseUnit "ElectronVolt"
    , testAssertSingle CaloriePerMole parseUnit "Ca/mol"
    , testAssertSingle CaloriePerMole parseUnit "Calorie/mol"
    , testAssertSingle CaloriePerMole parseUnit "Ca"
    , testAssertSingle CaloriePerMole parseUnit "Calorie"
    , testAssertSingle JoulePerMole parseUnit "J/mol"
    , testAssertSingle JoulePerMole parseUnit "Joule/mol"
    , testAssertSingle JoulePerMole parseUnit "J"
    , testAssertSingle JoulePerMole parseUnit "Joule"
    , testAssertSingle Kelvin parseUnit "K"
    , testAssertSingle Kelvin parseUnit "Kelvin"
    , testAssertSingle Wavenumber parseUnit "cm-1"
    , testAssertSingle Wavenumber parseUnit "Cm-1"
    , testAssertSingle Meter parseUnit "m"
    , testAssertSingle Meter parseUnit "Meter"
    , testAssertSingle Hertz parseUnit "Hertz"
    , testAssertSingle Hertz parseUnit "Hz"
    ]


testParseQuantity :: Test
testParseQuantity = TestList [
      testAssertSingle Quantity { number=114.514, prefix=None, unit=ElectronVolt } parseQuantity "114.514eV"
    ]


main :: IO Counts
main = do
    runTestTT testParseMetricPrefix
    runTestTT testParseUnit
    runTestTT testParseQuantity
