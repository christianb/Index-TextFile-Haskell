module Index
( index, printAsString) where

import Data.List (sortBy)
import Data.Char (toLower)

type Text = String
type Zeile = String
type Wort = String
type File = String

index :: [(Text,File)] -> [(Wort, [(File, [Int])])]
index content = sortMe (removeEmptyEntries (removeDoubleElements (mergeFiles (merge' (gather (changeStyleOfWords (ignoreHead (words' (addLn (ignoreTail (split content)))))))))))

removeEmptyEntries :: [(Wort, [(File, [Int])])] -> [(Wort, [(File, [Int])])]
removeEmptyEntries list = filter (\ a -> (fst a) /= "") list

split :: [(Text,File)] -> [([Zeile],File)]
split list = [split' pair | pair <- list]
    --where split' p = (lines (fst p), snd p)

-- trennt text in zeilen auf
split' :: (Text,File) -> ([Zeile],File)
split' pair = (lines (fst pair), snd pair)

ignoreTail :: [([Zeile],File)] -> [([Zeile],File)]
ignoreTail list = [(ignoreTail' (fst pair), snd pair) | pair <- list]

ignoreTail' :: [Zeile] -> [Zeile]
ignoreTail' [] = []
ignoreTail' (z:zs) = ignoreTail'' z:ignoreTail' zs

ignoreTail'' :: Zeile -> Zeile
ignoreTail'' [] = []
ignoreTail'' (c:cs)
    | isTailValid (c:[]) = c:(ignoreTail'' cs) 
    | otherwise = ' ':ignoreTail'' cs
    
-- Test if the tail of a word is valid
isTailValid :: String -> Bool
isTailValid [] = True
isTailValid (c:cs)
    | isAnyCharValid = (True && isTailValid cs)
    | otherwise = False
    where isAnyCharValid = isHeadValid c || c == '-' || (c >= '0' && c <= '9')


addLn :: [([Zeile],File)] -> [([(Zeile,Int)],File)]
addLn list = [(addLn' (fst pair), snd pair) | pair <- list]

-- fuege Zeilennummern hinzu
addLn' :: [Zeile] -> [(Zeile,Int)]
addLn' z = zip z [1..]

changeStyleOfWords :: [([(Wort,Int)],File)] -> [(Wort, (File, Int))]
changeStyleOfWords [] = []
changeStyleOfWords (list:lists) = changeStyleOfWords' (fst list) (snd list) ++ changeStyleOfWords lists

changeStyleOfWords' :: [(Wort,Int)] -> File -> [(Wort, (File, Int))]
changeStyleOfWords' pairs file = [changeStyleOfWords'' pair file | pair <- pairs]

changeStyleOfWords'' :: (Wort,Int) -> File -> (Wort, (File, Int))
changeStyleOfWords'' pair file = (fst pair, (file, snd pair))


words' :: [([(Zeile,Int)],File)] -> [([(Wort,Int)],File)]
words' list = [(words'' (fst pair), snd pair) | pair <- list]

-- erstelle eine liste mit wörtern und deren zeilennummer
words'' :: [(Zeile,Int)] -> [(Wort,Int)]
words'' [] = []
words'' (p:ps) = (zip wort_list (replicate (length wort_list) (snd p))) ++ words'' ps
    where wort_list = words (fst p)

ignoreHead :: [([(Wort,Int)],File)] -> [([(Wort,Int)],File)]
ignoreHead list = [(ignoreHead' (fst pair), snd pair) | pair <- list]

ignoreHead' :: [(Wort,Int)] -> [(Wort,Int)]
ignoreHead' [] = []
ignoreHead' list = [(ignoreHead'' (fst pair), snd pair) | pair <- list]

ignoreHead'' :: Wort -> Wort
ignoreHead'' [] = []
ignoreHead'' (c:cs)
    | isHeadValid (c) = c:cs
    | otherwise = ignoreHead'' cs

-- Test if the head of a word is valid
isHeadValid :: Char -> Bool
isHeadValid x
    | x == '_' = True
    | x >= 'A' && x <= 'Z' = True
    | x >= 'a' && x <= 'z' = True
    | isUmlaut x = True
    | otherwise = False
    
isUmlaut :: Char -> Bool
isUmlaut x
    | x == 'ä' || x == 'Ä' = True
    | x == 'ö' || x == 'Ö' = True
    | x == 'ü' || x == 'Ü' = True
    | x == 'ß' = True
    | otherwise = False
    
gather :: [(Wort, (File, Int))] -> [[(Wort, (File, Int))]]
gather list = (gather' list [])

gather' :: [(Wort, (File, Int))] -> [[(Wort, (File, Int))]] -> [[(Wort, (File, Int))]]
gather' [] temp = temp
gather' list temp 
    | isWordInTempList = gather' (tail list) temp
    | otherwise = gather' (tail list) ((collectSameWords (fst (head list)) list) : temp)
    where isWordInTempList = (elem (fst (head list)) (map fst (map head temp)))
    
collectSameWords :: Wort -> [(Wort, (File, Int))] -> [(Wort, (File, Int))]
collectSameWords word list = filter (\ e -> (word) == (fst e)) list

merge' :: [[(Wort, (File, Int))]] -> [(Wort, [(File, Int)])] 
merge' list = [merge'' sub_list | sub_list <- list]

merge'' :: [(Wort, (File, Int))] -> (Wort, [(File, Int)])
merge'' list = (fst (head list), (map snd list))

mergeFiles :: [(Wort, [(File, Int)])]  -> [(Wort, [(File, [Int])])]
mergeFiles [] = []
mergeFiles list = mergeFiles' (head list) : mergeFiles (tail list)

mergeFiles' :: (Wort, [(File, Int)]) -> (Wort, [(File, [Int])])
mergeFiles' pair = (fst pair, mergeFiles'' (snd pair) [])

mergeFiles'' :: [(File, Int)] -> [(File, [Int])] -> [(File, [Int])]
mergeFiles'' [] temp = temp
mergeFiles'' pairs temp
    | isFileInTempList = mergeFiles'' (tail pairs) temp
    | otherwise = mergeFiles'' (tail pairs) ( (fst (head pairs), collectSameFiles (fst (head pairs)) (pairs) ) : temp)
    where isFileInTempList = (elem (fst (head pairs)) (map fst temp))
    
collectSameFiles :: File -> [(File, Int)] -> [Int]
collectSameFiles file list = map snd (filter (\ e -> (file) == (fst e)) list)

removeDoubleElements :: [(Wort, [(File, [Int])])] -> [(Wort, [(File, [Int])])]
removeDoubleElements list = [(fst pair, removeDoubleElements' (snd pair)) | pair <- list]

removeDoubleElements' :: [(File, [Int])] -> [(File, [Int])]
removeDoubleElements' list = [removeDoubleElements'' pair | pair <- list]

removeDoubleElements'' :: (File, [Int]) -> (File, [Int])
removeDoubleElements'' pair = (fst pair, (removeDoubleElements'''( reverse (snd pair)) []) )

removeDoubleElements''' :: [Int] -> [Int] -> [Int]
removeDoubleElements''' [] temp = temp
removeDoubleElements''' (l:list) temp 
    | (elem l temp) = removeDoubleElements''' list temp
    | otherwise = removeDoubleElements''' list (l:temp)

sortMe :: [(Wort, [(File, [Int])])] -> [(Wort, [(File, [Int])])]
sortMe list = sortBy (\ x y -> compareMe (fst x) (fst y)) list

compareMe :: Wort -> Wort -> Ordering
compareMe [] [] = EQ
compareMe [] _ = LT
compareMe _ [] = GT
compareMe w1 w2
    | ord_case_insensitive == EQ = compareMe (tail w1) (tail w2)
        --if ord_case_sensitive == EQ
            --then compareMe (tail w1) (tail w2)
            --else ord_case_sensitive
    | otherwise = ord_case_insensitive
    where ord_case_insensitive
            | (isUmlaut (head w1) && isUmlaut (head w2)) = compareMe (replaced_w1 ++ (tail w1)) (replaced_w2 ++ (tail w2))
            | isUmlaut (head w1) = compareMe (replaced_w1 ++ (tail w1)) w2
            | isUmlaut (head w2) = compareMe w1 (replaced_w2 ++ (tail w2))
            | otherwise = compareInsensitive (head w1) (head w2)
            where replaced_w1 = replaceUmlautInWort (head w1)
                  replaced_w2 = replaceUmlautInWort (head w2)
          {-ord_case_sensitive
            | isUmlaut (head w1) && isUmlaut (head w2) = compareMe (replaced_w1 ++ (tail w1)) (replaced_w2 ++ (tail w2))
            | isUmlaut (head w1) = compareMe (replaced_w1 ++ (tail w1)) w2
            | isUmlaut (head w2) = compareMe w1 (replaced_w2 ++ (tail w2))
            | otherwise = compareSensitive (head w1) (head w2)
            where replaced_w1 = replaceUmlautInWort (head w1)
                  replaced_w2 = replaceUmlautInWort (head w2)-}
          
-- case insensitive compare
compareInsensitive :: Char -> Char -> Ordering
compareInsensitive c1 c2 = compare (toLower c1) (toLower c2)

-- case sensitive compare
compareSensitive :: Char -> Char -> Ordering
compareSensitive c1 c2 = compare c1 c2

containUmlaute :: Wort -> Bool
containUmlaute [] = False
containUmlaute (w:wort)
    | isUmlaut w = True
    | otherwise = containUmlaute wort

replaceUmlautInWort :: Char -> Wort
replaceUmlautInWort c
    | c == 'ä' = "ae"
    | c == 'Ä' = "Ae"
    | c == 'ö' = "oe"
    | c == 'Ö' = "Oe"
    | c == 'ü' = "ue"
    | c == 'Ü' = "Ue"
    | c == 'ß' = "ss"
    | otherwise = c:[]


printAsString :: [(Wort, [(File, [Int])])] -> String
printAsString [] = ""
printAsString (l:list) = ((fst l) ++ (printFileListAsString (snd l)) ++ (printAsString list))

printFileListAsString :: [(File, [Int])] -> String
printFileListAsString [] = ""
printFileListAsString (list:lists) = (" " ++ (printFileAsString (fst list) (snd list)) ++ "\n" ++ (printFileListAsString lists))

printFileAsString :: File -> [Int] -> String
printFileAsString file list = (file ++ (printLineNrAsString list))

printLineNrAsString :: [Int] -> String
printLineNrAsString [] = ""
printLineNrAsString (l:list) = (" ") ++  (show l) ++ (printLineNrAsString list)