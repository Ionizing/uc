module Main where

import Library

main :: IO ()
main = do
    putStrLn $ show $ parseMetricPrefix' 'k'
