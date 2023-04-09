> module Angabe4 where

1. Vervollstaendigen Sie gemaess Angabentext!
2. Loeschen Sie keine Deklarationen aus diesem Rahmenprogramm!
3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!


Aufgabe A.1


> type Nat0   = Int
> type Ebene  = Nat0
> type Breite = Nat0
> type Text   = String
> data BBaum  = Blatt Text
>               | Knoten Text BBaum BBaum deriving (Eq,Show)
> data Auswertung = Ausw Breite [Ebene] deriving (Eq,Show)

> breitest :: BBaum -> Auswertung
> breitest tree = Ausw maxWidth index
>         where maxWidth = maximum $ levelList tree
>               index = findMaxIndices $ levelList tree

> findMaxIndices :: [Int] -> [Int]
> findMaxIndices xs = [ i | (x,i) <- zip xs [0..], maxVal == x]
>                 where maxVal = maximum xs

> levelList :: BBaum -> [Int]
> levelList a = map (countAtLevel a) [0 .. (findLevels a)]

> findLevels :: BBaum -> Int
> findLevels (Blatt _) = 0
> findLevels (Knoten _ l r) = 1 + maximum [findLevels l, findLevels r]

> countAtLevel :: BBaum -> Int -> Int
> countAtLevel _ 0 = 1
> countAtLevel (Blatt _) n
>     | n > 0 = 0
>     | otherwise = 0
> countAtLevel (Knoten _ l r) n
>     | n > 0 = countAtLevel l (n-1) + countAtLevel r (n-1)
>     | otherwise = 0

> -- NUR ZUM DEBUGGEN
> countAllNodes :: BBaum -> Int
> countAllNodes (Blatt _) = 1
> countAllNodes (Knoten _ l r) = 1 + countAllNodes l + countAllNodes r


breitest geht folgendermassen vor: ...
    breitest Funktion: Bestimmt Baumebene mit den meisten (mindestens so viele wie auf jeder anderen Ebene) Nodes --> Ebene 0 = Wurzel
    findMaxIndices: die Indizes von dem höchsten Wert in Liste finden
    levelList: Liste in der an Stelle 1 die Nodes in Level 1 sind usw
    findLevels: findet Baumtiefe - 1 -> gesamtanzahl der levels



Aufgabe A.2


> tae :: BBaum -> Ebene -> Maybe [Text]
> tae tree n
>         | null list = Nothing
>         | otherwise = Just list
>         where list = valsAtLevel tree n

> valsAtLevel :: BBaum -> Int -> [String]
> valsAtLevel (Blatt val) n
>     | n == 0 = [val]
>     | n > 0 = []
>     | otherwise = []
> valsAtLevel (Knoten val l r) n
>     | n == 0 = [val]
>     | n > 0 = valsAtLevel l (n-1) ++ valsAtLevel r (n-1)
>     | otherwise = []

tae geht folgendermassen vor: ...
    Sie ist eine leichte Abänderung der vorherigen Funktion

Aufgabe A.3

> data TBaum    = TB
>                 | TK TBaum TBaum TBaum deriving (Eq,Show)
> data Richtung = L | M | R deriving (Eq,Show)
> type Weg      = [Richtung]
> data TBaum'   = TB' Weg
>                 | TK' Weg TBaum' TBaum' TBaum' deriving (Eq,Show)

> awi :: TBaum -> TBaum'
> awi x = transform x []

> transform :: TBaum -> Weg -> TBaum'
> transform TB w = TB' w 
> transform (TK l m r) w = TK' w (transform l (w ++ [L])) (transform m ( w ++ [M])) (transform r (w ++ [R]))

awi geht folgendermassen vor: ...
    Erstellt ein TBaum' rekursiv und füllt Weg basierend auf Rekursionsebene




Aufgabe A.4


> type Info a = [a]
> data Baum a = B (Info a)
>               | K (Baum a) (Info a) (Baum a)

> instance Show a => Show (Baum a) where
>    show (B var) = "<" ++ show var ++ ">"
>    show (K var1 var2 var3) = "<Wurzel " ++ show var2 ++ " " ++ show var1 ++ " " ++  show var3 ++ ">"

> instance Eq a => Eq (Baum a) where
>    B {} == K {} = False
>    K {} == B {} = False
>    (B var1) == (B var2) = var1 == var2 
>    (K var1a var2a var3a) == (K var1b var2b var3b) = (var2a == var2b) && (var1a == var1b) && (var3a == var3b)

> instance Ord a => Ord (Baum a) where
>   (>)  (B {}) (B {}) = False -- kein echter Anfangsbaum
>   (>)  (K {}) (B {}) = False
>   (>)  (B var1) (K _ var2 _) = sameContent var1 var2 && length var1 > length var2  
>   (>)  (K l1 var1 r1) (K l2 var2 r2) = sameContent var1 var2 && length var1 > length var2 && l1 > l2 && r1 > r2

>   (<)  (B {}) (B {}) = False -- kein echter Anfangsbaum
>   (<)  (K {}) (B {}) = False
>   (<)  (B var1) (K _ var2 _) = sameContent var1 var2 && length var1 < length var2  
>   (<)  (K l1 var1 r1) (K l2 var2 r2) = sameContent var1 var2 && length var1 < length var2 && l1 < l2 && r1 < r2

>   (>=) a b = (a == b) || (a > b)

>   (<=) a b = (a == b) || (a < b)

> sameContent :: (Eq a) => [a] -> [a] -> Bool -- überprüft nicht gleichheit sondern nur inhalte   
> sameContent _ [] = False
> sameContent [] _ = False
> sameContent (x:xs) (y:ys) = (x == y) && (sameContent xs ys)


Die Instanzdeklarationen gehen folgendermassen vor: ...
    Sie löst die Aufgabe.



Aufgabe A.5


> type UntereSchranke = Int
> type ObereSchranke  = Int
> data Intervall      = IV (UntereSchranke,ObereSchranke)
>                       | Leer
>                       | Ungueltig

> instance Show Intervall where
>         show Ungueltig = "Kein Intervall"
>         show Leer = "<>"
>         show (IV (a, b))
>                 | a > b  = "<>"
>                 | otherwise = "<" ++ show(a) ++ "," ++ show(b) ++ ">"
