module Angabe2 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. LÃ¶schen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0 = Integer


-- Aufgabe A.1

data IstEinsBinaerPrim = Ja | Nein deriving (Eq,Show) -- eigentlich Bool 

ist_einsbp :: Nat0 -> IstEinsBinaerPrim
ist_einsbp = boolMe.isPrime.sumOfList.toBin

toBin :: Integral a => a -> [a]
toBin 0 = [0]
toBin n = reverse $ calc2bin n -- effizienter als "++" zu benutzen

calc2bin :: Integral a => a -> [a]
calc2bin 0 = []
calc2bin n = (n `mod` 2) : calc2bin (n `div` 2)

sumOfList :: Integral a => [a] -> a -- man könnte auch sum von Prelude module verwenden
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs

isPrime :: Integral a => a -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime p = foldl (\acc x -> acc * x `mod` p) 1 [2..(p-2)] == 1

boolMe :: Bool -> IstEinsBinaerPrim
boolMe True = Ja
boolMe False = Nein

{- isPrime geht folgendermassen vor:
   Es wird links-gefalten mit dem anonymen Lambda Ausdruck (\acc x -> acc * x `mod` p),
   dem akkumulator 1,
   und der Liste [2..(p-2)]

   Beispiel: p = 8, p-2 = 6
   isPrime p = foldl (\acc x -> acc * x `mod` p) 1 [2..(p-2)] == 1
   isPrime 8 = foldl (\acc x -> acc * x `mod` 14) 1 [2..6] == 1

   erste Runde: acc = 1, x = 2
       1 * 2 `mod` 8 = 2
   zweite Runde: acc = 2, x = 3
       2 * 3 `mod` 8 = 6
   dritte Runde: acc = 6, x = 4
       6 * 4 `mod` 8 = 0
   vierte Runde: acc = 0, x = 5
       0 * 5 `mod` 8 = 0
   fünfte Runde: acc = 0, x = 6 (letzte Runde)
       0 * 6 `mod` 8 = 0
   
   Endgültiger Vergleich: 0 == 1 => FALSE
   8 ist keine Primzahl
-}

{- ist_einsbp geht folgendermassen vor: ... 
   wandelt die Zahl um in eine binäre Liste
   die Einsen werden aufaddiert
   und es wird festgestellt ob das Resultat eine Primzahl ist
   und abschließend wird ein Bool in das Ja/Nein Format umgewandelt
-}


-- Aufgabe A.2

type Von           = Nat0
type Bis           = Nat0
type VonBis        = (Von,Bis)
type Anzahl0bps    = Int
type Anzahl1bps    = Int
type Anzahlen01bps = (Anzahl0bps,Anzahl1bps)

anz01bps :: VonBis -> Anzahlen01bps
anz01bps (a,b) 
            | a > b     =  (-1,-1)
            | otherwise = (anz0bps(a,b), anz1bps(a,b))


anz1bps :: VonBis -> Anzahl1bps
anz1bps (a,b) = fromIntegral.sum $ map (bool2Bin.isPrime1) [a..b]

anz0bps :: VonBis -> Anzahl0bps
anz0bps (a,b) = fromIntegral.sum $ map (bool2Bin.isPrime0) [a..b]

-- Einser-Primzahl
isPrime1 :: Integer -> Bool
isPrime1 = isPrime.sumOfList.toBin

-- Nuller-Primzahl
isPrime0 :: Integer -> Bool
isPrime0 = isPrime.sumOfList.invertBin.toBin

-- invertiere Binärzahl
invertBin :: Integral a => [a] -> [a] 
invertBin [] = []
invertBin (x:xs)
   | x == 0 = 1 : invertBin xs
   | x == 1 = 0 : invertBin xs
   | otherwise = error "non binary in invertBin()"

-- Bool zu Binärzahl
bool2Bin :: Bool -> Integer
bool2Bin False = 0
bool2Bin True  = 1


{- anz01bps geht folgendermassen vor: ...
    für isPrime1 werden die vorherigen Methoden benutzt
    für isPrime0 werden die Bits geflipped und ebenso die vorherigen Methoden benutzt

    da das Output aber ein Bool ist wird sie auf 0,1 abgebildet
-}


-- Aufgabe A.3

type Wort      = String
type Wortliste = [Wort]

-- wird in lwi implementiert
liefere_woerter_in :: String -> Wortliste
liefere_woerter_in = lwi

lwi :: Wort -> [Wort]
lwi = words

spaces = "        "
l_test = "aa bb cc \t dd \n ee"


-- meine Implementierung von words
{-
lwi :: String -> Wortliste
lwi [] = [""]
lwi (x:xs)
    | x == ' ' = "" : rest -- "" darf an einem [String] angehängt werden
    | x == '\n' = "" : rest
    | x == '\t' = "" : rest
    | otherwise = (x : head rest) : tail rest -- weil x nur an einem String angehängt werden kann und an keinem [String]
    where
        rest = lwi xs

example :: [[Char]]
example = ('x' : "") : ["test"]
-}


{- lwi geht folgendermassen vor: ... 
    Es wird x von (x:xs) mit den Trenn-Zeichen verglichen.
    Wenn x ein Trenn-Zeichen ist, wird "" an der Berechnung von xs angehängt
    Ansonsten wird x an der Berechnung von xs angehängt

         (x : head rest)   ---> STRING
         tail rest         ---> [STRING]

--> Gute Zusammenfassung mit einfachem code: https://jmmv.dev/2006/08/split-function-in-haskell.html
-}


-- Aufgabe A.4

type Hammingabstand = Int

hM :: [String] -> Hammingabstand
hM [] = 0
hM input
    | length input == 1 = 0
    | otherwise = minimum [h inp_i inp_j | (inp_i,i) <- zip input [0..], (inp_j,j) <- zip input [0..], i /= j] -- vergleicht quasi alle Einträge aus einem 2D Array, außer x == y


h :: String -> String -> Int -- Aus vorherigem Arbeitsblatt
h [] [] = 0
h [] _  = -1
h  _ [] = -1
h wort1 wort2
  | length wort1 /= length wort2 = -1
  | head wort1   == head wort2   = 0 + h (tail wort1) (tail wort2)
  | otherwise                    = 1 + h (tail wort1) (tail wort2)



t1 = hM ["Fahrrad", "Autobus"] -- 7
t2 = hM ["1001", "1111", "1100"] -- 2
t3 = hM ["Haskell", "Fortran", "Miranda", "Clojure"] -- 6
t4 = hM["Haskell", "Java", "Prolog"] -- -1


{- hM geht folgendermassen vor: ... 
    es werden aus dem Input 2 Tupel Listen gemacht:
        (inp_i, i)
        (inp_j, j)
    Dabei geben i und j jeweils die Indizes eines zweidimensionalen Arrays, mit Hammingabständen an

Alternativer Zugang: (Mit logischen Operatoren)
-- https://gist.github.com/seaneshbaugh/4c6f06b73d106f0e0e5a#file-hammingdistance-hs-L3
-}
