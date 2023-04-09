module Angabe7 where
   
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

-- Funktionen ----------------------------------------------------------------------------------------------------------------------------

-- Aufgabe A.1
-- akt_band
akt_band :: Band -> Bandfeld -> Bandalphabet -> Band
akt_band function input output = (\i -> if i == input then output else function i)

-- akt_rechenband
akt_rechenband :: Rechenband -> Bandfeld -> Bandalphabet -> Rechenband
akt_rechenband (RB U f) inp out = RB (B inp inp) (akt_band f inp out) -- leer
akt_rechenband (RB (B min max) f) inp out
               | out /= Blank = RB (B min max) newFunc-- Grenzen ändern sich nicht weil kein Eintrag gelöscht wird
               | min == max && min == inp && out == Blank = RB U newFunc
               | inp == min && out == Blank = RB (B (findNextBiggest f inp max) max) newFunc
               | inp == max && out == Blank = RB (B min (findNextSmallest f inp min)) newFunc
               | otherwise = error "error in akt_rechenband"
            where
               newFunc = akt_band f inp out

findNextBiggest :: Band -> Bandfeld -> Bandfeld -> Bandfeld
findNextBiggest func i max
   | func i /= Blank && i <= max = i
   | i > max = error "error in findNextBiggest"
   | otherwise = findNextBiggest func (i+1) max

findNextSmallest :: Band -> Bandfeld -> Bandfeld -> Bandfeld
findNextSmallest func i min
   | func i /= Blank && i >= min = i
   | i < min = error "error in findNextSmallest"
   | otherwise = findNextSmallest func (i-1) min

-- Aufgabe A.2
-- (a) Komforteingabe
wandle_in_rb :: [Zeichenvorrat] -> Rechenband
wandle_in_rb "" = RB U blankFunc
wandle_in_rb str = RB (B 0 (toInteger $ length str - 1)) (str2b blankFunc str 0)

blankFunc :: Band
blankFunc = const Blank

str2b :: Band -> String -> Integer -> Band -- aufrufen mit blankFunc STRING 0 
str2b b "" _ = b
str2b b str pos = str2b (akt_band b pos (Z $ str !! fromIntegral pos)) str (pos+1)

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
transition g@(GZ t rb@(RB _ band) iz pos)
            | null t = GZ t rb iz pos -- tafel leer
            | cmdLn /= Nothing = executeCmd g $ (\(Just (_,_,b,_)) -> b) cmdLn  -- exekutiert befehl aus der Zeile
            | otherwise = GZ t rb iz pos -- keine befehle zum ausführen
         where
            cmdLn = getCmdln t iz (band pos)

-- muss ich hier getrennt machen weil ich vollständige Tafel returnen muss falls nichts gefunden
getCmdln :: Turingtafel -> IZ -> LSKZ -> Maybe Zeile
getCmdln [] _ _ = Nothing
getCmdln (x@(i,a,_,_):xs) iz lskz
            | i == iz && a == lskz = Just x
            | otherwise = getCmdln xs iz lskz

-- für pattern matching bei befehlen
executeCmd :: GZustand -> Befehl -> GZustand
executeCmd (GZ t rb iz pos) (Bewege_LSK_nach Links) = GZ t rb iz (pos-1) -- ändert pos
executeCmd (GZ t rb iz pos) (Bewege_LSK_nach Rechts)  =  GZ t rb iz (pos+1) -- ändert pos
executeCmd (GZ t rband iz pos) (Drucke char) = GZ t (akt_rechenband rband pos char) iz pos -- ändert rechenband

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
   show (SA iz pos (RB U _)) = "IZ: " ++ show iz ++ " // LSKP: " ++ show pos ++ " BI: Leer"
   show (SA iz pos (RB (B min max) band)) = "IZ: " ++ show iz ++ " // LSKP: " ++ show pos ++ " BI: " ++ show min ++ ">" ++ show (b2str band min max) ++ "<" ++ show max

-- Test Cases -------------------------------------------------------------------------------------------------------------------------------

-- akt_band
b1 :: Band
b1 x 
   | x >= 2 = Z '|'
   | x < 10 = Z '|'
   | otherwise = Blank

b2 :: Band
b2 = (\x -> Blank)

b1Updated :: Band
b1Updated = akt_band b1 1 (Z '|')

b2Updated :: Band
b2Updated = akt_band b2 1 (Z '|')

-- akt_rechenband

-- (a) Komforteingabe

-- (b) Zulässige Turingtafel

-- (c) Zustandsübergangsfunktion (transition)

-- (d) Spur (spur)

-- (e) Zustandsausgabe (zeige zustand)

-- (f) Spurausgabe (zeige spur)

-- (g) Truingmaschinensimulator (sim)

-- (h) Simulatorausgabe (instance Show Sim Ausgabe)

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
