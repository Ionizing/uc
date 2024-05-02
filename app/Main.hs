module Main where

import Text.Parsec
import Library

main :: IO ()
main = do
    parseTest parseQuantity "114.514nm"
