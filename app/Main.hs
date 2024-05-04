module Main where

import Data.List (intercalate)
import Text.Printf (
      printf
    , PrintfArg
    , formatArg
    , fmtChar
    , fmtWidth
    , fmtPrecision
    , formatString
    )
import Options.Applicative
import Library


data App = App [Quantity] String
    deriving (Eq, Show)

app :: Parser App
app = App
    <$> some (argument auto (
        metavar "QUANTITIES" <> help "E.g. '1.0eV', '20THz' and '3.14Kcm-1'..."
    ))
    <*> option str (
        metavar "FORMAT" <> long "fmt" <> short 'f' <> value "%#8.3q"
    <>  help "Format the output. Strings like '%10.5q' and '%10.5q' are available."
    )


data Header = Header
instance PrintfArg Header where
    formatArg _ fmt = formatString totStr totFmt
        where
        totFmt = fmt {fmtChar='s', fmtWidth=Nothing, fmtPrecision=Nothing}
        strFmt = fmt {fmtChar='s', fmtPrecision=Nothing}
        colStr s = formatString s strFmt ""
        totStr = first ++ other
        first  = colStr "INPUT" ++ " | "
        other  = intercalate " | " $ map colStr [
              "ElectronVolt"
            , "Calorie/mol"
            , "Joule/mol"
            , "Temperature"
            , "Hartree"
            , "Wavenumber"
            , "Wavelength"
            , "Frequency"
            ]


formatQuantities :: String -> Quantity -> String
formatQuantities fmt q = originalStr ++ " = " ++ formattedStr
    where
    originalStr  = printf fmt q
    formattedStr = intercalate " | " $ map (printf fmt) converted
    converted    = map convertQuantity [
          ElectronVolt
        , CaloriePerMole
        , JoulePerMole
        , Kelvin
        , Hartree
        , Wavenumber
        , Meter
        , Hertz
        ] <*> pure q


run :: App -> IO ()
run (App xs fmt) = putStrLn $ headerStr ++ "\n" ++ bodyStr
    where
    headerStr = printf fmt Header :: String
    bodyStr   = intercalate "\n" $ map (formatQuantities fmt) xs


main :: IO ()
main = run =<< execParser opts
    where
    opts = info (app <**> helper <**> simpleVersioner "v0.0.1")
        ( fullDesc
        <> header "Unit conversion toy for quantum chemistry researchers."
        <> footer (
            "Available units: 'eV Cal/mol J/mol K(Kelvin) Ha(Hartree) cm-1 m(meter) Hz'."
        ++  " Prefixes from 'a'(Atto) to 'E'(Exa) are also available."
        ++  " In order to eliminate the confusions,"
        ++  " the prefixes smaller than unity (from 'a'(Atto,10^-18) to 'm'(Milli,10^-3)) should be lowercased,"
        ++  " while the larger prefixes (from 'K'(Kilo,10^3) to 'E'(Exa,10^18)) should be uppercased."
        ++  " For example: '1meV' means '1 Milli ElectronVolt' while '1MeV' stands for '1 Mega ElectronVolt'"
        )
        )
