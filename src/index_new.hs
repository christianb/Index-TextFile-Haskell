type Text = String
type Zeile = String
type Wort = String
type File = String

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

merge :: [([(Wort,[Int])],File)] -> [(Wort,(File, [Int]))]
merge [] = []
merge (list:lists) = merge' (fst list) (snd list) ++ merge lists

merge' :: [(Wort,[Int])] -> File -> [(Wort,(File, [Int]))] 
merge' list file = [(fst pair, (file, snd pair)) | pair <- list]

gather :: [([(Wort,Int)],File)] -> [([[(Wort,Int)]],File)]
gather list = [(gather' (fst pair) [], snd pair)| pair <- list]

-- kombiniere die zeilennummern eines wortes in eine liste
gather' :: [(Wort,Int)] -> [[(Wort,Int)]]-> [[(Wort,Int)]]
gather' [] combined = combined
gather' list combined 
    | not (isInList (head list) combined) = gather' (tail list) ((collectSameWords (head list) list) : combined)
    | otherwise = gather' (tail list) combined
    
isInList :: (Wort,Int) -> [[(Wort,Int)]] -> Bool
isInList _ [] = False
isInList pair combined = (elem (fst pair) (map fst (map head combined)))

collectSameWords :: (Wort,Int) -> [(Wort,Int)] -> [(Wort,Int)]
collectSameWords pair list = filter (\ e -> (fst pair) == (fst e)) list

combine :: [([[(Wort,Int)]],File)] -> [([(Wort,[Int])],File)]
combine list = [(combine' (fst pair), snd pair) | pair <- list]

combine':: [[(Wort,Int)]] -> [(Wort,[Int])]
combine' [] = []
combine' list = (fst (head (head list)), (map snd (head list))) : (combine' (tail list))
{-
merge :: [([(Wort,[Int])],File)] -> [(Wort, [(File, [Int])])]
merge list = [(fst pair,  | inner_pair <- [(fst pair | pair <- list]]

merge' :: -}