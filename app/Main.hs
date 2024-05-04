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
        metavar "QUANTITIES"
     <> help "E.g. '1.0eV', '20THz' and '3.14Kcm-1'..."
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
        <> header "Unit conversion toy for quantum chemistry researchers."
        <> footer (
            "Available units: 'eV Cal/mol J/mol K(Kelvin) Ha(Hartree) cm-1 m(meter) Hz'."
        ++  " Prefixes from 'a(Atto)' to 'E(Exa)' are also available."
        ++  " In order to eliminate the confusions,"
        ++  " the prefixes smaller than unity (from 'a(Atto)' to 'm(Milli-)') should be lowercased,"
        ++  " while the larger prefixes (from 'K(Kilo)' to 'E(Exa)') should be uppercased."
        ++  " For example: '1meV' means '1 Milli ElectronVolt' while '1MeV' stands for '1 Mega ElectronVolt'"
        )
        )
