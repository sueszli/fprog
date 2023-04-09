module Angabe1 where

type Nat1 = Int

-- Aufgabe A.1

-- Testfälle aus der Angabe 
test1_1 :: [a]
test1_1 = []
test1_2 :: [Int]
test1_2 = [42]
test1_3 :: [Int]
test1_3 = [4, -2, 5, 4, 3, -2, 5, 4, -12]

filtere :: Nat1 -> [Int] -> [Int]
filtere _ []  = []
filtere n xs  = revQuicksort . nub' $ [a | a <- xs, (occurencesOfx a xs) == n] -- vielfachen entfernt und sortiert

occurencesOfx :: Ord a => a -> [a] -> Int -- # des Vorkommens eines Elementes aus einer Liste
occurencesOfx _ [] = 0
occurencesOfx n xs = (length . filter (== n)) xs -- Komposition: berechnet länge von gefilterten Listen

{-
Alternative Implementierung:

occurencesOfx  :: Ord a => a -> [a] -> Int
occurencesOfx  _ [] = 0
occurencesOfx  n (x:xs)
    | n == x = 1 + occurencesOfx (n-1) xs
    | otherwise = occurencesOfx n xs
-}

revQuicksort :: (Ord a) => [a] -> [a] -- sortiert Listen absteigend
revQuicksort [] = []
revQuicksort (x:xs) =
  let biggerSorted = revQuicksort [a | a <- xs, a >= x]
      smallerSorted = revQuicksort [a | a <- xs, a < x]
  in  biggerSorted ++ [x] ++ smallerSorted

nub' :: Eq a => [a] -> [a] -- entfernt alle vielfachen aus der Liste m (ist eigentlich im Data.List Modul)
nub' [] = []
nub' (x:xs) = x : filter (/= x) (nub' xs)

-- Aufgabe A.2

-- Testfälle aus der Angabe 
test2_1 :: [a]
test2_1 = []
test2_2 :: [(Int, Int)]
test2_2 = [(2,3),(3,4),(5,6)]

kommt_vor :: Int -> [(Int,Int)] -> Bool
kommt_vor _ [] = False
kommt_vor n (x:xs)
  | n == fst x = True -- überprüfe erstes Element im Tupel
  | n == snd x = True -- überprüfe zweites Element im Tupel
  | otherwise  = kommt_vor n xs

-- Aufgabe A.3

-- Testfälle aus der Angabe 
test3_1 :: [a]
test3_1 = []
test3_2 :: [Int]
test3_2 = [42]
test3_3 :: [Int]
test3_3 = [4, -2, 5, 4, 3, -2, 5, 4, -12]

aus :: [Int] -> [Int] -- <A>uffüllen <U>nd <S>ortieren
aus []  = []
aus xs = 
    let maxOcc = maxOccurences xs -- höchste wiederholungsanzahl eines elementes
        pureList = nub' xs -- liste ohne wiederholungen der Elemente
    in  quicksort $ repeatEach_nTimes maxOcc pureList

repeatEach_nTimes :: Int -> [Int] -> [Int] -- alle elemente der liste werden ver-n-facht
repeatEach_nTimes _ [] = []
repeatEach_nTimes n [x] = replicate n x
repeatEach_nTimes n (x:xs) = replicate n x ++ repeatEach_nTimes n xs

maxOccurences :: Ord a => [a] -> Int -- höchste wiederholungsanzahl eines elementes
maxOccurences [] = 0
maxOccurences xs = findMax [ occurencesOfx a xs | a <- xs]

findMax :: [Int] -> Int -- wäre eigentlich nicht notwendig weil man maximum auf listen verwenden kann
findMax []  = 0
findMax [x] = x
findMax (x:xs) 
 | (findMax xs) > x = findMax xs
 | otherwise = x

quicksort :: (Ord a) => [a] -> [a] -- sortiert Listen aufsteigend
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted

-- Aufgabe A.4

h :: String -> String -> Int
h [] [] = 0
h [] _  = -1
h  _ [] = -1
h wort1 wort2
  | length wort1 /= length wort2 = -1
  | head wort1   == head wort2   = 0 + h (tail wort1) (tail wort2)
  | otherwise                    = 1 + h (tail wort1) (tail wort2)