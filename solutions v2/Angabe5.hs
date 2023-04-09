module Angabe5 where

{- 1. Vervollstaendigen Sie gemaess Angabentext!
   2. Löschen Sie keine Deklarationen aus diesem Rahmenprogramm, auch nicht die Modulanweisug!
   3. Achten Sie darauf, dass Gruppe Leserechte fuer Ihre Abgabedatei hat!
-}

type Nat0                = Int
type Nat1                = Int
type Vorname             = String
type Nachname            = String
data Partei              = ABC | DEF | GHI | JKL | MNO deriving (Eq,Show)
data Wahlwerber          = WW Vorname Nachname Partei deriving (Eq,Show)
type Wahlvorschlag       = [Wahlwerber]
type Wahlvorschlagsplatz = Nat1
type Wahlsieger          = (Wahlwerber,Wahlvorschlagsplatz)
type Stimmzettel         = [Wahlvorschlagsplatz]
type Wahl                = [Stimmzettel]
type Gueltig             = [Stimmzettel]
type Ungueltig           = [Stimmzettel]
type Platz_1_Stimmen     = Nat0
data Wahlausgang         = Ungueltiger_Wahlvorschlag
                           | Keine_gueltigen_Stimmen
                           | Gewaehlt_ist Wahlwerber
                           | Kein_Wahlsieger_Wahlwiederholung deriving (Eq,Show)
data Groesste_Wahlverlierer = GWV [Partei]
                             | Keine 
                             | Analyse_nicht_moeglich deriving (Eq,Show)


-- Aufgabe A.1

ist_gueltiger_Wahlvorschlag :: Wahlvorschlag -> Bool
ist_gueltiger_Wahlvorschlag list = not $ null list

{- ist_gueltiger_Wahlvorschlag geht folgendermassen vor: ... 
-}


-- Aufgabe A.2
{-
meine Interpretation:
   Länge von Wahlvorschlag = k   
   -- Wahlvorschlag muss gültig sein 
   -- Anzahl der Zahlen <= k
   -- Werte der Zahlen >= 1 und <= k
   -- Keine Wiederholung der Elemente in Zahlen Liste erlaubt
   -- Leere Stimmzettel erlaubt
-}

ist_gueltiger_Stimmzettel :: Wahlvorschlag -> Stimmzettel -> Bool
ist_gueltiger_Stimmzettel [] [] = True
ist_gueltiger_Stimmzettel _ [] = True
ist_gueltiger_Stimmzettel candidates vote =
      (length vote <= length candidates) && checkDuplicates vote && checkRange vote (length candidates)

checkDuplicates :: Stimmzettel -> Bool
checkDuplicates [] = True
checkDuplicates (x:xs) = x `notElem` xs && checkDuplicates xs

checkRange :: Stimmzettel -> Int -> Bool
checkRange xs k = foldl (\acc x -> acc && (x >= 1) && (x <= k)) True xs

{- ist_gueltiger_Stimmzettel geht folgendermassen vor: ... 
-}
 

-- Aufgabe A.3

trenne_Stimmzettel :: Wahlvorschlag -> Wahl -> (Gueltig,Ungueltig)
trenne_Stimmzettel candidate allVotes = (getGueltig candidate allVotes, getUngueltig candidate allVotes)

getGueltig :: Wahlvorschlag -> Wahl -> Gueltig 
getGueltig candidates = foldr (\x acc -> if ist_gueltiger_Stimmzettel candidates x then x:acc else acc) []

getUngueltig :: Wahlvorschlag -> Wahl -> Ungueltig 
getUngueltig candidates = foldr (\x acc -> if ist_gueltiger_Stimmzettel candidates x then acc else x:acc) []

{- trenne_Stimmzettel geht folgendermassen vor: ... 
-}
 

-- Aufgabe A.4

auszaehlen :: Wahlvorschlag -> Wahl -> Maybe [Platz_1_Stimmen]
auszaehlen [] _ = Nothing
auszaehlen candidate allVotes
               | foldr (\x acc -> null x && acc) True allVotes = Nothing
               | not $ isValid candidate allVotes = Nothing
               | otherwise = Just (map (countHeads allVotes) [1 .. (length candidate)])

isValid :: Wahlvorschlag -> Wahl -> Bool
isValid candidates = foldl (\acc x -> acc && ist_gueltiger_Stimmzettel candidates x) True

countHeads :: Wahl -> Int -> Int
countHeads xs k = foldl (\acc x -> acc + (if head x == k then 1 else 0)) 0 xs

{- auszaehlen geht folgendermassen vor: ... 
-}


-- Aufgabe A.5

wahlsieger :: Wahlvorschlag -> Maybe [Platz_1_Stimmen] -> Maybe Wahlsieger
wahlsieger [] _ = Nothing
wahlsieger _ Nothing = Nothing
wahlsieger candidates (Just p1)
                           | null candidates = Nothing
                           | length candidates /= length p1 = Nothing
                           | maximum p1 <= sum p1 `div` 2  = Nothing
                           | otherwise = clearWinner candidates (zip p1 [1..])

clearWinner :: Wahlvorschlag -> [(Platz_1_Stimmen, Int)] -> Maybe Wahlsieger
clearWinner [] _ = Nothing
clearWinner _ [] = Nothing
clearWinner candidates p1@(x:xs)
                           | fst x > total `div` 2 = Just (candidates !! (snd x - 1), snd x)
                           | otherwise = clearWinner candidates xs
                           where total = sum $ map fst p1

{- wahlsieger geht folgendermassen vor: ... 
-}


-- Aufgabe A.6

ausscheiden :: Wahl -> [Platz_1_Stimmen] -> Wahl
ausscheiden allVotes p1 = map (remove deadIndexes) allVotes
                  where deadIndexes = findDeadIndexes allVotes p1 0

remove :: [Int] -> Stimmzettel -> Stimmzettel
remove deads = foldr (\x acc -> if x `elem` deads then acc else x:acc ) []

-- findet das kleinste Index dass sowohl in platz1stimmen ist, als auch in wahlzettel.
findDeadIndexes :: Wahl -> [Platz_1_Stimmen] -> Int -> [Int]
findDeadIndexes allVotes p1 min
            | min `elem` p1 && indexesElemOfAllVotes allVotes minIndexes = minIndexes
            | otherwise = findDeadIndexes allVotes p1 (min+1) -- there are dead candidates among p1
            where 
               minIndexes = findIndexesOfVal p1 1 min

indexesElemOfAllVotes :: Wahl -> [Int] -> Bool
indexesElemOfAllVotes allVotes minIndexes = 
   foldr (\index acc -> inner index allVotes || acc ) False minIndexes

inner :: Int -> Wahl -> Bool
inner index allVotes = foldr (\vote acc -> (index `elem` vote) || acc) False allVotes

-- find indexes of a specific value in an int list
findIndexesOfVal :: [Platz_1_Stimmen] -> Int -> Int -> [Int]
findIndexesOfVal [] _ _ = []
findIndexesOfVal (x:xs) i min
                     | x == min  = i : findIndexesOfVal xs (i+1) min
                     | otherwise = findIndexesOfVal xs (i+1) min

{- ausscheiden geht folgendermassen vor: ... 
-}


-- Aufgabe A.7

wahlausgang :: Wahlvorschlag -> Wahl -> Wahlausgang
wahlausgang [] _ = Ungueltiger_Wahlvorschlag
wahlausgang _ [] = Kein_Wahlsieger_Wahlwiederholung
wahlausgang candidates allVotes
                           | null validVotes = Keine_gueltigen_Stimmen
                           | isNull = Kein_Wahlsieger_Wahlwiederholung --[[],[],[]]
                           | wahlsieger candidates p1 /= Nothing = Gewaehlt_ist winner
                           | otherwise = wahlausgang candidates $ ausscheiden validVotes $ conv p1
                     where
                           validVotes = getGueltig candidates allVotes
                           p1 = auszaehlen candidates validVotes
                           winner = fst $ conv $ wahlsieger candidates p1
                           isNull = foldr (\x acc -> null x && acc) True allVotes

conv :: Maybe a -> a
conv (Just a) = a

{- wahlausgang eht folgendermassen vor: ... 
-}


-- Aufgabe A.8

wahlanalyse :: Wahlvorschlag -> Wahl -> Groesste_Wahlverlierer
wahlanalyse candidates allVotes 
               | inValidResults = Analyse_nicht_moeglich
               | numOfParties == 1 = Keine
               | otherwise = GWV $ map (parties!!) ind
            where
               inValidResults = wahlausgang candidates allVotes `elem` [Ungueltiger_Wahlvorschlag, Kein_Wahlsieger_Wahlwiederholung, Keine_gueltigen_Stimmen]
               numOfParties = length $ removeDuplicates parties
               parties = getParties candidates
               ind = map pred $ findDeadIndexes allVotes (conv $ auszaehlen candidates (getGueltig candidates allVotes)) 0

getParties :: Wahlvorschlag -> [Partei] 
getParties = foldr (\(WW _ _ p) acc -> p:acc) []

removeDuplicates :: [Partei] -> [Partei]
removeDuplicates = foldr (\x acc -> if x `elem` acc then acc else x:acc ) []


{- wahlanalyse geht folgendermassen vor: ... 
-}
