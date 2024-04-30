module Main where

import Library (
    MetricPrefix (..)                       -- (..) means importing all data constructors
  , parseMetricPrefix
    )
import Test.HUnit
import qualified System.Exit as Exit
import Text.Parsec.String
import Text.Parsec


testSingle :: (Eq a, Show a) => a -> (Parser a) -> String -> String -> Test
testSingle expect pfun instr msg = TestCase $ let
    res = case (parse pfun msg instr) of
        Right a -> a
        Left  a -> error $ "test failed: " ++ msg
    in res @?= expect


testParseMetricPrefix :: Test
testParseMetricPrefix = TestList [
      testSingle Atto  parseMetricPrefix "a" "should return Atto"
    , testSingle Femto parseMetricPrefix "f" "should return Femto"
    , testSingle Pico  parseMetricPrefix "p" "should return Pico"
    , testSingle Nano  parseMetricPrefix "n" "should return Nano"
    , testSingle Micro parseMetricPrefix "u" "should return Micro"
    , testSingle Milli parseMetricPrefix "m" "should return Milli"
    , testSingle Kilo  parseMetricPrefix "K" "should return Kilo"
    , testSingle Mega  parseMetricPrefix "M" "should return Mega"
    , testSingle Giga  parseMetricPrefix "G" "should return Giga"
    , testSingle Tera  parseMetricPrefix "T" "should return Tera"
    , testSingle Peta  parseMetricPrefix "P" "should return Peta"
    , testSingle Exa   parseMetricPrefix "E" "should return Exa"
    ]

main :: IO()
main = do
    result <- runTestTT testParseMetricPrefix
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
