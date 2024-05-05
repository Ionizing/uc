# uc (Unit Converter)

A toy for the energy unit conversion used in quantum chemistry, written in Haskell.

## Build

You need to install Glasgow Haskell Compiler (GHC) 9.8.2 (maybe other version is
also OK, but I never tried) and `cabal` to build the binary. GHC and cabal can
be installed by [GHCup](https://www.haskell.org/ghcup/).

Then use cabal to build
```
$ cd uc
$ cabal build
```
and the binary should be in `dist-newstyle/build/x86_64-osx/ghc-x.x.x/uc-x.x.x.x/x/uc/build/uc/uc`

## Usage

```text
> uc --help
Unit conversion toy for quantum chemistry researchers.

Usage: uc QUANTITIES [-f|--fmt FORMAT]

Available options:
  QUANTITIES               E.g. '1.0eV', '20THz' and '3.14Kcm-1'...
  -f,--fmt FORMAT          Format the output. Strings like '%10.5q' and '%10.5q'
                           are available.
  -h,--help                Show this help text
  --version                Show version information

Available units: 'eV Cal/mol J/mol K(Kelvin) Ha(Hartree) cm-1 m(meter) Hz'.
Prefixes from 'a'(Atto) to 'E'(Exa) are also available. In order to eliminate
the confusions, the prefixes smaller than unity (from 'a'(Atto,10^-18) to
'm'(Milli,10^-3)) should be lowercased, while the larger prefixes (from
'K'(Kilo,10^3) to 'E'(Exa,10^18)) should be uppercased. For example: '1meV'
means '1 Milli ElectronVolt' while '1MeV' stands for '1 Mega ElectronVolt'
```

Example:

- Ordinary situation
```text
> uc 3.14eV 50THz 1Kcm-1 650nm
       INPUT | ElectronVolt |  Calorie/mol |    Joule/mol |  Temperature |      Hartree |   Wavenumber |   Wavelength |    Frequency
------------ | ------------ | ------------ | ------------ | ------------ | ------------ | ------------ | ------------ | ------------
    3.140 eV =       3.140  |      72.412K |     302.972K |      36.438K |     115.393m |      25.326K |     394.854n |     759.249T
  50.000 THz =     206.783m |       4.769K |      19.952K |       2.400K |       7.599m |       1.668K |       5.996μ |      50.000T
 1.000 Kcm-1 =     123.981m |       2.859K |      11.963K |       1.439K |       4.556m |       1.000K |      10.000μ |      29.979T
  650.000 nm =       1.907  |      43.988K |     184.046K |      22.135K |      70.097m |      15.385K |     650.000n |     461.219T
```

- You can also specify the output format
```text
> uc -f "%15.4q" 3.14eV 50THz 1Kcm-1 650nm
          INPUT |    ElectronVolt |     Calorie/mol |       Joule/mol |     Temperature |         Hartree |      Wavenumber |      Wavelength |       Frequency
--------------- | --------------- | --------------- | --------------- | --------------- | --------------- | --------------- | --------------- | ---------------
      3.1400 eV =         3.1400  |        72.4121K |       302.9721K |        36.4382K |       115.3928m |        25.3264K |       394.8541n |       759.2486T
    50.0000 THz =       206.7834m |         4.7687K |        19.9521K |         2.3996K |         7.5991m |         1.6679K |         5.9958μ |        50.0000T
   1.0000 Kcm-1 =       123.9813m |         2.8592K |        11.9627K |         1.4387K |         4.5562m |         1.0000K |        10.0002μ |        29.9786T
    650.0000 nm =         1.9074  |        43.9880K |       184.0458K |        22.1350K |        70.0974m |        15.3850K |       650.0000n |       461.2191T
```

- If you want the unit to be displayed right after the number, just use the alternate sign `#` in the format string
```text
> uc -f "%#15.3q" 3.14eV 50THz 1Kcm-1 650nm
          INPUT |    ElectronVolt |     Calorie/mol |       Joule/mol |     Temperature |         Hartree |      Wavenumber |      Wavelength |       Frequency
--------------- | --------------- | --------------- | --------------- | --------------- | --------------- | --------------- | --------------- | ---------------
       3.140 eV =        3.140 eV | 72.412 KCal/mol |  302.972 KJ/mol |       36.438 KK |     115.393 mHa |    25.326 Kcm-1 |      394.854 nm |     759.249 THz
     50.000 THz =     206.783 meV |  4.769 KCal/mol |   19.952 KJ/mol |        2.400 KK |       7.599 mHa |     1.668 Kcm-1 |        5.996 μm |      50.000 THz
    1.000 Kcm-1 =     123.981 meV |  2.859 KCal/mol |   11.963 KJ/mol |        1.439 KK |       4.556 mHa |     1.000 Kcm-1 |       10.000 μm |      29.979 THz
     650.000 nm =        1.907 eV | 43.988 KCal/mol |  184.046 KJ/mol |       22.135 KK |      70.097 mHa |    15.385 Kcm-1 |      650.000 nm |     461.219 THz
```
