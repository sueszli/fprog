> module Angabe3 where

> type UntereSchranke = Int
> type ObereSchranke  = Int
> data Intervall  = IV (UntereSchranke,ObereSchranke)
>                       | Leer
>                       | Ungueltig

Allgemeine Hilfsfunktionen:

> istLeer :: Intervall -> Bool
> istLeer (IV (a,b)) = if a > b then True else False
> istLeer Leer = True
> istLeer Ungueltig = False

> istIV :: Intervall -> Bool
> istIV Ungueltig = False
> istIV Leer = False
> istIV _ = True

> istUng :: Intervall -> Bool
> istUng Ungueltig = True
> istUng _ = False

> fst' :: Intervall -> Int
> fst' (IV (a,_)) = a

> snd' :: Intervall -> Int
> snd' (IV (_,b)) = b

> length' :: Intervall -> Int
> length' a
>   | istLeer a = 0
>   | otherwise = snd' a - fst' a

> toList :: Intervall -> [Int]
> toList Leer = []
> toList (IV (a,b))
>   | a > b = []
>   | otherwise = [a..b]
> toList Ungueltig = error "Falscher Aufruf @ toList"   

Aufgabe A.1

> instance Show Intervall where
>   show Ungueltig = "Kein Intervall"
>   show Leer = "<>"
>   show (IV (a, b))
>       | a > b = show Leer
>       | a == b = show a
>       | otherwise = "<" ++ show a ++ "," ++ show b ++ ">"

Aufgabe A.2

> instance Eq Intervall where
>   Ungueltig == _ = error "Vergleich nicht moeglich"
>   _ == Ungueltig = error "Vergleich nicht moeglich"
>   Leer == iv
>       | istLeer iv = True
>       | otherwise = False
>   iv == Leer
>       | istLeer iv = True
>       | otherwise = False
>   (IV (a, b)) == (IV (c, d))
>       | (a > b) && (c > d) = True -- beide Leer
>       | (a == c) && (b == d) = True -- gleich
>       | otherwise = False

Aufgabe A.3

> instance Ord Intervall where
>   a `compare` b
>       | a == b = EQ
>       | istLeer a = LT
>       | istLeer b = GT
>       | fst' a >= fst' b && snd' a <= snd' b = LT
>       | fst' a <= fst' b && snd' a >= snd' b = GT
>       | otherwise = error "Vergleich nicht moeglich"

Aufgabe A.4

> multiplyFirsts :: Intervall -> Intervall -> Int
> multiplyFirsts x y = minimum [(fst' x * fst' y), (fst' x * snd' y), (snd' x * fst' y), (snd' x * snd' y)]

> multiplySeconds :: Intervall -> Intervall -> Int
> multiplySeconds x y = maximum [(fst' x * fst' y), (fst' x * snd' y), (snd' x * fst' y), (snd' x * snd' y)]

> instance Num Intervall where
>   Ungueltig + _ = error "Vergleich nicht moeglich"
>   _ + Ungueltig = error "Vergleich nicht moeglich"
>   Leer + _ = Leer
>   _ + Leer = Leer
>   (IV a) + (IV b)
>       | (istLeer (IV a)) && (istLeer (IV b)) = Leer
>       | otherwise = (IV (fst' (IV a) + fst' (IV b), snd' (IV a) + snd' (IV b)))

>   Ungueltig - _ = error "Vergleich nicht moeglich"
>   _ - Ungueltig = error "Vergleich nicht moeglich"
>   Leer - _ = Leer
>   _ - Leer = Leer
>   (IV (a, b)) - (IV (c, d))
>       | a > b || c > d  = Leer
>       | otherwise = IV(a - d, b - c)

>   Ungueltig * _ = error "Vergleich nicht moeglich"
>   _ * Ungueltig = error "Vergleich nicht moeglich"
>   Leer * _ = Leer
>   _ * Leer = Leer
>   (IV a) * (IV b) 
>       | (istLeer (IV a)) && (istLeer (IV b)) = Leer
>       | otherwise = IV(multiplyFirsts (IV a) (IV b), multiplySeconds (IV a) (IV b))

>   negate a
>       | istLeer a = Leer
>       | istIV a = IV((-1) * fst' a, (-1) * snd' a) 
>       | otherwise = error "Vergleich nicht moeglich"

>   abs a
>       | istLeer a = Leer
>       | istIV a = if (fst' a < 0) && (snd' a < 0) then (IV (abs $ snd' a, abs $ fst' a)) else a
>       | istIV a = if (fst' a < 0) && (snd' a > 0) then (IV (abs $ fst' a, abs $ snd' a)) else a
>       | istIV a = if (fst' a > 0) && (snd' a < 0) then (IV (abs $ fst' a, abs $ snd' a)) else a
>       | otherwise = error "Vergleich nicht moeglich"

>   signum a
>       | istIV a || istLeer a = Leer
>       | otherwise = error "Vergleich nicht moeglich"

>   fromInteger _ = error "Vergleich nicht moeglich"

Aufgabe A.5

> instance Enum Intervall where
>    succ a
>        | istIV a && (fst' a == snd' a) = (IV (fst' a + 1, fst' a + 1))
>        | otherwise = error "Operation nicht moeglich"
>    pred a
>        | istIV a && (fst' a == snd' a) = (IV (fst' a - 1, fst' a - 1))
>        | otherwise = error "Operation nicht moeglich"
>    fromEnum a
>        | istIV a && (fst' a == snd' a) = fst' a
>        | otherwise = error "Operation nicht moeglich"
>    toEnum a = IV(a, a)

Aufgabe A.6

> class Kanonisch a where
>  kanonisch :: a -> a

> instance Kanonisch Intervall where
>   kanonisch Ungueltig = Ungueltig
>   kanonisch Leer = Leer
>   kanonisch (IV (a,b)) = if a > b then Leer else (IV (a,b))

Aufgabe A.7

> class Element a where
>  is_elem :: Int -> a -> Maybe Bool

> instance Element Intervall where
>    is_elem i a
>        | istLeer a = Just False
>        | istIV a = if i `elem` (toList a) then Just True else Just False
>        | otherwise = Nothing

Aufgabe A.8

> class Element a => Code a where
>  codiere :: [Int] -> a
>  decodiere :: a -> Maybe [Int]

> instance Code Intervall where
>    decodiere a -- Intervall zu Liste
>        | istLeer a = Just []
>        | istIV a = Just [(fst' a), (fst' a + 1).. (snd' a)]
>        | otherwise = Nothing
>    codiere list -- Liste zu Intervall
>        | list == [] = Leer
>        | ist_lueckenlos list && ist_aufsteigend list = (IV (head list, last list))
>        | otherwise = Ungueltig   

Aufgabe A.9

> class (Ord a,Enum a) => ExTest a where
>  extrahiere  :: Maybe [a] -> [a]
>  ist_aufsteigend :: [a] -> Bool
>  ist_lueckenlos :: [a] -> Bool
>  ist_laL_Element :: a -> Maybe [a] -> Bool

> instance ExTest Int where
>    extrahiere (Just a) = a
>    extrahiere Nothing = error "Extraktion nicht moeglich"
>    ist_aufsteigend [] = True
>    ist_aufsteigend [_] = True
>    ist_aufsteigend (x:y:xs) = x < y && ist_aufsteigend (y:xs)
>    ist_lueckenlos list = hasElems (findMinMax list) (list)  
>    ist_laL_Element _ Nothing = False
>    ist_laL_Element i (Just list)
>        | (ist_aufsteigend list) && (ist_lueckenlos list) = elem i list
>        | otherwise = False

> hasElems :: [Int] -> [Int] -> Bool
> hasElems [] _ = True
> hasElems [x] ys = x `elem` ys
> hasElems (x:xs) ys = (x `elem` ys) && (xs `hasElems` ys)

> findMinMax :: [Int] -> [Int]
> findMinMax [] = []
> findMinMax [x] = [x]
> findMinMax xs = [(head xs)..(last xs)]
