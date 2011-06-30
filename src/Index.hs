module Index
(
index
, print'
) where

import Data.List (sortBy)
import Data.Char (toLower)

type Text = String
type Zeile = String
type Wort = String
type File = String

index :: [(Text,File)] -> [(Wort, [(File, [Int])])]
index content = sortMe (mergeFiles (merge' (gather (changeStyleOfWords (ignoreHead (words' (addLn (ignoreTail (split content)))))))))

split :: [(Text,File)] -> [([Zeile],File)]
split list = [split' pair | pair <- list]
    --where split' p = (lines (fst p), snd p)

-- trennt text in zeilen auf
split' :: (Text,File) -> ([Zeile],File)
split' pair = (lines (fst pair), snd pair)

{-
ignore :: [([Zeile],File)] -> [([Zeile],File)]
ignore list = [(ignore' (fst pair), snd pair) | pair <- list]

ignore' :: [Zeile] -> [Zeile]
ignore' list = ignoreHead (ignoreTail list)
-}

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
    | x == 'ä' || x == 'Ä' = True
    | x == 'ö' || x == 'Ö' = True
    | x == 'ü' || x == 'Ü' = True
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
    
--merge'' :: (Wort, [(File, Int)]) -> (Wort, [(File, [Int])])
--merge pair = (fst pair, merge''' (snd pair))
{-
merge''' :: [(File, Int)] -> [(File, [Int])] -> [(File, [Int])]
merge''' [] temp = temp
merge''' list temp 
    | isFileInTempList = merge''' (tail list) temp
    | otherwise = merge''' (tail list) (collectSameFiles (fst (head list)) list : temp)
    where isFileInTempList = (elem (fst (head list)) (map fst temp))
-}
collectSameFiles :: File -> [(File, Int)] -> [Int]
collectSameFiles file list = map snd (filter (\ e -> (file) == (fst e)) list)

sortMe :: [(Wort, [(File, [Int])])] -> [(Wort, [(File, [Int])])]
sortMe list = sortBy (\ x y -> compareMe (fst x) (fst y)) list

compareMe :: Wort -> Wort -> Ordering
compareMe [] [] = EQ
compareMe [] _ = LT
compareMe _ [] = GT
compareMe w1 w2
    | ord_case_insensitive == EQ = 
        if ord_case_sensitive == EQ
            then compareMe (tail w1) (tail w2)
            else ord_case_sensitive
    | otherwise = ord_case_insensitive
    where ord_case_insensitive = compareMe' (head w1) (head w2)
          ord_case_sensitive = compareMe'' (head w1) (head w2)
          
-- case insensitive compare
compareMe' :: Char -> Char -> Ordering
compareMe' c1 c2 = compare (toLower c1) (toLower c2)

-- case sensitive compare
compareMe'' :: Char -> Char -> Ordering
compareMe'' c1 c2 = compare c1 c2


print' :: [(Wort, [(File, [Int])])] -> IO ()
print' (list:lists) = do 
    printWord (fst list)
    --putStr " "
    printFileList (snd list)
    print' lists

printWord :: String -> IO ()
printWord word = do
    putStr word

printFileList :: [(File, [Int])] -> IO ()
printFileList [] = return ()
printFileList (list:lists) = do
    putStr " "
    printFile (fst list) (snd list)
    printFileList lists

printFile :: File -> [Int] -> IO ()
printFile file list = do
    putStr file
    printLineNr list
    putStrLn ""
    
printLineNr :: [Int] -> IO ()
printLineNr [] = return ()
printLineNr (l:list) = do
    putStr " "
    putStr (show l)
    printLineNr list