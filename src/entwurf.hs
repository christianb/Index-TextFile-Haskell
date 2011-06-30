import System.Environment   
import Data.List    
import System.Directory  
import System.IO

type Text = String
type Zeile = String
type Wort = String
type File = String

-- trennt text in zeilen auf
split :: Text -> [Zeile]
split t = lines t

-- Test if the head of a word is valid
isHeadValid :: Char -> Bool
isHeadValid x
    | x == '_' = True
    | x >= 'A' && x <= 'Z' = True
    | x >= 'a' && x <= 'z' = True
    | x == 'ä' || x == 'Ä' = True
    | x == 'ö' || x == 'Ö' = True
    | x == 'ü' || x == 'Ü' = True
    | otherwise = False

-- Test if the tail of a word is valid
isTailValid :: String -> Bool
isTailValid [] = True
isTailValid (c:cs)
    | isAnyCharValid = (True && isTailValid cs)
    | otherwise = False
    where isAnyCharValid = isHeadValid c || c == '-' || (c >= '0' && c <= '9')

ignore :: [Zeile] -> [Zeile]
ignore list = ignoreHead (ignoreTail list)

-- ungueltige Zeichen durch leerzeichen ersetzen
ignoreTail :: [Zeile] -> [Zeile]
ignoreTail [] = []
ignoreTail (z:zs) = ignoreCharTail z:ignoreTail zs

ignoreCharTail :: Zeile -> Zeile
ignoreCharTail [] = []
ignoreCharTail (c:cs)
    | isTailValid (c:[]) = c:(ignoreCharTail cs) 
    | otherwise = ' ':ignoreCharTail cs
    
ignoreHead :: [Zeile] -> [Zeile]
ignoreHead [] = []
ignoreHead (z:zs) = ignoreCharHead z:ignoreHead zs

ignoreCharHead :: Zeile -> Zeile
ignoreCharHead [] = []
ignoreCharHead (c:cs)
    | isHeadValid (c) = c:(ignoreCharHead cs)
    | otherwise = ' ':ignoreCharHead cs

-- fuege Zeilennummern hinzu
addLn :: [Zeile] -> [(Zeile,Int)]
addLn z = zip z [1..]

-- erstelle eine liste mit wörtern und deren zeilennummer
words' :: [(Zeile,Int)] -> [(Wort,Int)]
words' [] = []
words' (p:ps) = (zip wort_list (replicate (length wort_list) (snd p))) ++ words' ps
    where wort_list = words (fst p)

combine :: [[(Wort,Int)]] -> [(Wort,[Int])]
combine [] = []
combine list = (fst (head (head list)), (map snd (head list))) : (combine (tail list))

-- kombiniere die zeilennummern eines wortes in eine liste
gather :: [(Wort,Int)] -> [[(Wort,Int)]]-> [[(Wort,Int)]]
gather [] combined = combined
gather list combined 
    | not (isInList (head list) combined) = gather (tail list) ((collectSameWords (head list) list) : combined)
    | otherwise = gather (tail list) combined

-- collect all same wods from list
collectSameWords :: (Wort,Int) -> [(Wort,Int)] -> [(Wort,Int)]
collectSameWords pair list = filter (\ e -> (fst pair) == (fst e)) list

isInList :: (Wort,Int) -> [[(Wort,Int)]] -> Bool
isInList _ [] = False
isInList pair combined = (elem (fst pair) (map fst (map head combined)))

addFileName :: [(Wort,[Int])] -> File -> [(Wort,(File,[Int]))]
addFileName list file = [((fst p), (file, (snd p))) | p <- list]

appendFileListsAndTupels :: [[(Wort,(File,[Int]))]] -> [(Wort,(File,[Int]))]
appendFileListsAndTupels [] = []
appendFileListsAndTupels (a:as) = a ++ appendFileListsAndTupels as

gatherFileList :: [(Wort,(File,[Int]))] -> [[(Wort,(File,[Int]))]]
gatherFileList list = (filter (\a -> (fst a) == fst (head list)) list) 

combineFileList :: [(Wort,(File,[Int]))] -> [(Wort,[(File,[Int])])] -> [(Wort,[(File,[Int])])]
combineFileList list = gatherFileList (head list)

combineFileList' :: [(Wort,(File,[Int]))] -> [(File,[Int])]
combineFileList' [] = []
combineFileList' list = snd (head list):[] ++ combineFileList' (tail list)


isInFileList :: Wort -> [(Wort,[(File,[Int])])] -> Bool
isInFileList w list = elem w (map fst list)
{-
gatherFileListAndTupels :: [(Wort,(File,[Int]))] -> [[(Wort,(File,[Int]))]] -> [[(Wort,(File,[Int]))]]
gatherFileListAndTupels [] combined = combined
gatherFileListAndTupels list combined
    | not (isInFileList (head list) combined) = gatherFileListAndTupels (tail list) ((collectSameWords (head list) list) : combined)
    
isInFileList :: (Wort,(File,[Int])) -> [[(Wort,(File,[Int]))]] -> Bool
isInFileList _ [] = False
isInFileList pair combined = (elem (fst pair) (map fst (map head combined)))
-}
{-combineFileListsAndTupels :: [(Wort,(File,[Int]))] -> [(Wort,[(File,[Int])])]
combineFileListsAndTupels [] -> []
combineFileListsAndTupels list -> fst (head list) : combineFileListsAndTupels (tail list)
-}
{-- sortiere wörter
sort :: [(Wort,[Int])] -> [(Wort,[Int])]-}