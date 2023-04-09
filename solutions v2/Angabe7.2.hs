module Angabe7 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Löschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}

-- Natuerliche Zahlen, ganze Zahlen
type Nat0 = Integer
type Zett = Integer

-- Zeichenvorrat, Bandalphabet einer Turingmaschine
type Zeichenvorrat = Char
data Bandalphabet  = Z Zeichenvorrat | Blank deriving (Eq,Show)

-- Rechenband
type Min        = Zett -- Kleinster Index eines beschriebenen Bandfelds
type Max        = Zett -- Groesster Index eines beschriebenen Bandfelds
data MinMax     = B Min Max | U deriving (Eq,Show)     -- B wie `Beschrieben', U wie `Unbeschrieben'  
type Bandfeld   = Zett
type Band       = (Bandfeld -> Bandalphabet) 
data Rechenband = RB MinMax Band

-- Lese- und Schreibkopf (LSK)
type LSK_Position      = Zett
type Zeichen_unter_LSK = Bandalphabet
data Richtung          = Links | Rechts deriving (Eq,Show)
data Befehl            = Drucke Bandalphabet | Bewege_LSK_nach Richtung deriving (Eq,Show)

-- Interne Turingmaschinenzustaende
type Zustand               = Nat0
type Interner_Zustand      = Zustand
type Interner_Folgezustand = Zustand

-- Abkuerzungen
type LSKZ = Zeichen_unter_LSK
type IZ   = Interner_Zustand
type IFZ  = Interner_Folgezustand 

-- Turing-Tafeln
type Zeile       = (IZ,LSKZ,Befehl,IFZ)
type Turingtafel = [Zeile]

-- Globale Turingmaschinenzustaende
data GZustand = GZ Turingtafel Rechenband IZ LSK_Position

-- Spuren
type Spur = [GZustand]

-- Turingmaschinensimulatoreingabe
type Initiales_Rechenband = Rechenband
data Sim_Eingabe = SE Turingtafel Initiales_Rechenband

-- Turingmaschinensimulatorausgabe
type Finaler_interner_Zustand = Zustand
type Finale_LSK_Position      = LSK_Position
type Finales_Rechenband       = Rechenband

-- Abkuerzungen
type FIZ   = Finaler_interner_Zustand
type FLSKP = Finale_LSK_Position
type FRB   = Finales_Rechenband

-- Simulatorausgabe
data Sim_Ausgabe = SA FIZ FLSKP FRB


-- Aufgabe A.1
-- akt_band
akt_band :: Band -> Bandfeld -> Bandalphabet -> Band
akt_band function input output = (\i -> if i == input then output else function i)

-- akt_rechenband
akt_rechenband :: Rechenband -> Bandfeld -> Bandalphabet -> Rechenband
akt_rechenband rb@(RB U _) _ Blank = rb
akt_rechenband (RB U band) inp out = RB (B inp inp) (akt_band band inp out)
akt_rechenband (RB (B min max) band) inp out = RB getMinMax newBand
   where 
         newBand = akt_band band inp out
         getMinMax = if null getNonBlankIndex then U else B (head getNonBlankIndex) (last getNonBlankIndex)
         getNonBlankIndex = foldr (\x acc-> if newBand x /= Blank then x:acc else acc) [] [minimum[min,inp] .. maximum[max,inp]]

-- Aufgabe A.2
-- (a) Komforteingabe
wandle_in_rb :: [Zeichenvorrat] -> Rechenband
wandle_in_rb [] = RB U emptyBand
wandle_in_rb str = RB (B 1 (toInteger $ length str)) band'
   where
      band' = foldr (\(i,c) acc -> akt_band acc i (Z c)) emptyBand (zip [1..toInteger $ length str] str) -- von Johannes Zotterle

emptyBand :: Band
emptyBand = (\x -> Blank)

-- (b) Zulässige Turingtafel
-- Zulaessige Turingtafeln
ist_zulaessige_Turingtafel :: Turingtafel -> Bool
ist_zulaessige_Turingtafel tafel = not $ null tafel && allDifferent (map (\(a,b,_,_) -> (a, b)) tafel)
   
allDifferent :: (Eq a) => [a] -> Bool
allDifferent []     = True
allDifferent (x:xs) = x `notElem` xs && allDifferent xs

-- (c) Zustandsübergangsfunktion (transition)
-- Turingmaschinenzustandsuebergangsfunktion, kurz Transitionsfunktion
transition :: GZustand -> GZustand
transition g@(GZ t (RB _ band) iz pos)
            | null t = g -- tafel leer
            | cmdLn == Nothing = g -- keine befehle zum ausführen
            | cmdLn /= Nothing = executeCmd g $ (\(Just (_,_,b,j)) -> (b,j)) cmdLn  -- exekutiert befehl aus der Zeile
         where
            cmdLn = getCmdln t iz (band pos) -- zeile von tafel

-- muss ich hier getrennt machen weil ich vollständige Tafel returnen muss falls nichts gefunden
getCmdln :: Turingtafel -> IZ -> LSKZ -> Maybe Zeile
getCmdln [] _ _ = Nothing
getCmdln (x@(i,a,_,_):xs) iz lskz
            | i == iz && a == lskz = Just x
            | otherwise = getCmdln xs iz lskz

-- für pattern matching bei befehlen
executeCmd :: GZustand -> (Befehl,IFZ) -> GZustand
executeCmd (GZ t rb _ pos) ((Bewege_LSK_nach Links), j) = GZ t rb j (pos-1)
executeCmd (GZ t rb _ pos) ((Bewege_LSK_nach Rechts),  j) =  GZ t rb j (pos+1)
executeCmd (GZ t rband _ pos) ((Drucke char), j) = GZ t (akt_rechenband rband pos char) j pos

-- (d) Spur (spur)
spur :: GZustand -> Spur
spur g@(GZ t (RB _ band) iz pos)
   | terminate = [g]
   | otherwise = g : spur (transition g)
   where
      terminate = getCmdln t iz (band pos) == Nothing

-- (e) Zustandsausgabe (zeige zustand)
zeige_zustand :: GZustand -> String
zeige_zustand (GZ _ (RB U _) iz pos) = "(IZ:" ++ show iz ++ ",LSK:" ++ show pos ++ ",B:unbeschrieben)"
zeige_zustand (GZ _ (RB (B min max) band) iz pos) = "(IZ:" ++ show iz ++ ",LSK:" ++ show pos ++ ",B:"++ (b2str band min max) ++ ",Min:" ++ show min ++ ",Max:" ++ show max ++")"

b2str :: Band -> Integer -> Integer -> String
b2str f min max
         | out == Blank = ""
         | min == max && out /= Blank = [(\(Z char) -> char) out]
         | min < max  && out /= Blank = [(\(Z char) -> char) out] ++ b2str f (min+1) max
         | otherwise = error "error in b2str"
      where
         out = f min

-- (f) Spurausgabe (zeige spur)
zeige_spur :: Spur -> String
zeige_spur [] = "" 
zeige_spur [x] = zeige_zustand x
zeige_spur (x:xs) = zeige_zustand x ++ " ->> " ++ zeige_spur xs

-- (g) Truingmaschinensimulator (sim)
-- Turingmaschinensimulator, kurz Simulator
sim :: Sim_Eingabe -> Sim_Ausgabe
sim (SE t irb) = SA fiz fpos frb
   where
      (GZ _ frb fiz fpos) = last $ spur (GZ t irb 0 0) -- soll endlosschleife erzeugen wenn turing maschine nicht hält

-- (h) Simulatorausgabe (instance Show Sim Ausgabe)
-- Simulatorausgabe
instance Show Sim_Ausgabe where
   show (SA iz pos (RB U _)) = "IZ: " ++ show iz ++ " // LSKP: " ++ show pos ++ " // BI: Leer"
   show (SA iz pos (RB (B min max) band)) = "IZ: " ++ show iz ++ " // LSKP: " ++ show pos ++ " // BI: " ++ show min ++ ">" ++ (b2str band min max) ++ "<" ++ show max


-- Test Cases -------------------------------------------------------------------------------------------------------------------------------

-- zeige Zustand
tt :: Turingtafel
tt = [(0, Blank, Drucke (Z '|'), 1),
      (1, Z '|', Bewege_LSK_nach Links, 2)]

rb1 :: Rechenband
rb1 = wandle_in_rb "|||"

z1 :: GZustand
z1 = GZ tt rb1 1 0

test0 :: Bool
test0 = zeige_zustand z1 == "(IZ:1,LSK:0,B:|||,Min:1,Max:3)"

rb2 :: Rechenband
rb2 = wandle_in_rb ""
z2 :: GZustand
z2 = GZ tt rb2 0 1

test1 :: Bool
test1 = zeige_zustand z2 == "(IZ:0,LSK:1,B:unbeschrieben)"

band1 :: Rechenband
band1 = wandle_in_rb "|||||"

-- zeige_spur
spur1 :: Spur
spur1 = [z1,z1,z1]
text1 = "(IZ:1,LSK:0,B:|||,Min:1,Max:3)"

test2 = zeige_spur spur1 == text1 ++ " ->> " ++ text1 ++ " ->> " ++ text1

spur2 :: Spur
spur2 = [z1,z2]
text2 = "(IZ:0,LSK:1,B:unbeschrieben)"

test3 = zeige_spur spur2 == text1 ++ " ->> " ++ text2

minmax :: MinMax
minmax = B 0 2

rho :: Band
rho = \z -> if (0 <= z) && (z <= 2) then (Z '|') else Blank

rho_fin :: Rechenband
rho_fin = RB minmax rho

se :: Sim_Eingabe
se = SE [(0,Blank,Drucke (Z '|'),1),(1,Z '|',Bewege_LSK_nach Links,2)] (wandle_in_rb "||")

-- test4 = sim se == (SA 2 -1 rho_fin)

sa1 :: Sim_Ausgabe
sa1 = SA 2 4 rb1
test4 = show sa1 == "IZ: 2 // LSKP: 4 // BI: 1>|||<3"
sa2 :: Sim_Ausgabe
sa2 = SA 4 2 rb2

test5 = show sa2 == "IZ: 4 // LSKP: 2 // BI: Leer"
test6 = show (sim se) == "IZ: 2 // LSKP: -1 // BI: 0>|||<2"
testAll = test0 && test1 && test2 && test3 && test4 && test5 && test6

taf2 :: [(Integer, Bandalphabet, Befehl, Integer)]
taf2 = [(0, Blank  ,Bewege_LSK_nach Rechts,1),
         (1,Blank  ,Drucke Blank          ,8),
         (1,(Z '|'),Drucke Blank          ,2),
         (2,Blank  ,Bewege_LSK_nach Rechts,3),
         (3,(Z '|'),Bewege_LSK_nach Rechts,3),
         (3,Blank  ,Bewege_LSK_nach Rechts,4),
         (4,(Z '|'),Bewege_LSK_nach Rechts,4),
         (4,Blank  ,Drucke (Z '|')        ,5),
         (5,(Z '|'),Bewege_LSK_nach Rechts,5),
         (5,Blank  ,Drucke (Z '|')        ,6),
         (6,(Z '|'),Bewege_LSK_nach Links ,6),
         (6,Blank  ,Bewege_LSK_nach Links ,7),
         (7,(Z '|'),Bewege_LSK_nach Links ,7),
         (7,Blank  ,Bewege_LSK_nach Rechts,1)
         ]

testCase0 = transition (GZ taf2 band1 0 0)
testCase1 = zeige_spur $ spur (GZ taf2 band1 0 0)
testCase2 = sim (SE taf2 band1)