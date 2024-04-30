module Main where

import Library (
    MetricPrefix (..)                       -- (..) means importing all data constructors
  , parseMetricPrefix
    )
import Test.HUnit
import qualified System.Exit as Exit
import Text.Parsec.String
import Text.Parsec


testAssertSingle :: (Eq a, Show a) => a -> (Parser a) -> String -> String -> Test
testAssertSingle expect pfun instr msg = TestCase $ let
    res = case (parse pfun msg instr) of
        Right a -> a
        Left  a -> error $ "test failed: " ++ msg
    in res @?= expect


testParseMetricPrefix :: Test
testParseMetricPrefix = TestList [
      testAssertSingle Atto  parseMetricPrefix "a"    "should return Atto"
    , testAssertSingle Atto  parseMetricPrefix "atto" "should return Atto"
    , testAssertSingle Atto  parseMetricPrefix "Atto" "should return Atto"
    , testAssertSingle Femto parseMetricPrefix "f"     "should return Femto"
    , testAssertSingle Femto parseMetricPrefix "femto" "should return Femto"
    , testAssertSingle Femto parseMetricPrefix "Femto" "should return Femto"
    , testAssertSingle Pico parseMetricPrefix "p"    "should return Pico"
    , testAssertSingle Pico parseMetricPrefix "pico" "should return Pico"
    , testAssertSingle Pico parseMetricPrefix "Pico" "should return Pico"
    , testAssertSingle Nano parseMetricPrefix "n"    "should return Nano"
    , testAssertSingle Nano parseMetricPrefix "nano" "should return Nano"
    , testAssertSingle Nano parseMetricPrefix "Nano" "should return Nano"
    , testAssertSingle Micro parseMetricPrefix "u"     "should return Micro"
    , testAssertSingle Micro parseMetricPrefix "μ"     "should return Micro"
    , testAssertSingle Micro parseMetricPrefix "mu"    "should return Micro"
    , testAssertSingle Micro parseMetricPrefix "Mu"    "should return Micro"
    , testAssertSingle Micro parseMetricPrefix "micro" "should return Micro"
    , testAssertSingle Micro parseMetricPrefix "Micro" "should return Micro"
    , testAssertSingle Milli parseMetricPrefix "m"     "should return Milli"
    , testAssertSingle Milli parseMetricPrefix "milli" "should return Milli"
    , testAssertSingle Milli parseMetricPrefix "Milli" "should return Milli"
    , testAssertSingle Kilo parseMetricPrefix "K"    "should return Kilo"
    , testAssertSingle Kilo parseMetricPrefix "kilo" "should return Kilo"
    , testAssertSingle Kilo parseMetricPrefix "Kilo" "should return Kilo"
    , testAssertSingle Mega parseMetricPrefix "M"    "should return Mega"
    , testAssertSingle Mega parseMetricPrefix "mega" "should return Mega"
    , testAssertSingle Mega parseMetricPrefix "Mega" "should return Mega"
    , testAssertSingle Giga parseMetricPrefix "G"    "should return Giga"
    , testAssertSingle Giga parseMetricPrefix "giga" "should return Giga"
    , testAssertSingle Giga parseMetricPrefix "Giga" "should return Giga"
    , testAssertSingle Tera parseMetricPrefix "T"    "should return Tera"
    , testAssertSingle Tera parseMetricPrefix "tera" "should return Tera"
    , testAssertSingle Tera parseMetricPrefix "Tera" "should return Tera"
    , testAssertSingle Peta parseMetricPrefix "P"    "should return Peta"
    , testAssertSingle Peta parseMetricPrefix "peta" "should return Peta"
    , testAssertSingle Peta parseMetricPrefix "Peta" "should return Peta"
    , testAssertSingle Exa parseMetricPrefix "E"   "should return Exa"
    , testAssertSingle Exa parseMetricPrefix "exa" "should return Exa"
    , testAssertSingle Exa parseMetricPrefix "Exa" "should return Exa"
    ]

main :: IO()
main = do
    result <- runTestTT testParseMetricPrefix
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
