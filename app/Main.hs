module Main where

import Options.Applicative
--import Text.Parsec
--import Text.Printf
import Library


data App = App [Quantity]
    deriving (Eq, Show)

app :: Parser App
app = App
    <$> some (argument auto (
        metavar "INPUT"
     <> help "Input quantity. e.g. '1.0eV', '20THz' and '3.14Kcm-1'"
    ))


run :: App -> IO ()
run (App xs) = print xs


main :: IO ()
--main = do
    --parseTest parseQuantity "114.514nm"
    --printf "[%Q]\n" Quantity {unit=Meter, prefix=Nano, number=11.4514}
    --printf "[%q]\n" Quantity {unit=Meter, prefix=Nano, number=11.4514}
main = run =<< execParser opts
    where
    opts = info (app <**> helper <**> simpleVersioner "v0.0.1")
        ( fullDesc
        <> progDesc "Unit conversion toy for quantum chemistry researchers."
        <> footer "This is the footer"
        )
