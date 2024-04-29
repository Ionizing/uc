module Main where

import Library (
    MetricPrefix (..)                       -- (..) means importing all data constructors
  , parseMetricPrefix'
    )
import Test.HUnit
import qualified System.Exit as Exit

testParseMetricPrefix :: Test
testParseMetricPrefix = TestList [
      TestLabel "atto"  $ TestCase $ assertEqual "should return Just Atto"  Atto  (parseMetricPrefix' 'a')
    , TestLabel "femto" $ TestCase $ assertEqual "should return Just Femto" Femto (parseMetricPrefix' 'f')
    , TestLabel "pico"  $ TestCase $ assertEqual "should return Just Pico"  Pico  (parseMetricPrefix' 'p')
    , TestLabel "nano"  $ TestCase $ assertEqual "should return Just Nano"  Nano  (parseMetricPrefix' 'n')
    , TestLabel "micro" $ TestCase $ assertEqual "should return Just Micro" Micro (parseMetricPrefix' 'u')
    , TestLabel "milli" $ TestCase $ assertEqual "should return Just Milli" Milli (parseMetricPrefix' 'm')
    , TestLabel "Kilo"  $ TestCase $ assertEqual "should return Just Kilo"  Kilo  (parseMetricPrefix' 'K')
    , TestLabel "Kilo"  $ TestCase $ assertEqual "should return Just Kilo"  Kilo  (parseMetricPrefix' 'k')
    , TestLabel "Mega"  $ TestCase $ assertEqual "should return Just Mega"  Mega  (parseMetricPrefix' 'M')
    , TestLabel "Giga"  $ TestCase $ assertEqual "should return Just Giga"  Giga  (parseMetricPrefix' 'G')
    , TestLabel "Giga"  $ TestCase $ assertEqual "should return Just Giga"  Giga  (parseMetricPrefix' 'g')
    , TestLabel "Tera"  $ TestCase $ assertEqual "should return Just Tera"  Tera  (parseMetricPrefix' 'T')
    , TestLabel "Tera"  $ TestCase $ assertEqual "should return Just Tera"  Tera  (parseMetricPrefix' 't')
    , TestLabel "Peta"  $ TestCase $ assertEqual "should return Just Peta"  Peta  (parseMetricPrefix' 'P')
    , TestLabel "Exa"   $ TestCase $ assertEqual "should return Just Exa"   Exa   (parseMetricPrefix' 'E')
    ]

main :: IO()
main = do
    result <- runTestTT testParseMetricPrefix
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
