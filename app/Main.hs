module Main where

import Library

main :: IO ()
main = putStrLn $ show $ map parseMetricPrefix "afpnumKMGTPEs"
