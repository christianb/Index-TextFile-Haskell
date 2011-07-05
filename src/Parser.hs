import Data.Char(isSpace, isNumber)

type Text = String
type Zeile = String
type Wort = String
type File = String


parse :: String -> [(Wort, [(File, [Int])])]
parse content = parse' (lines content)

parse' :: [String] -> [(Wort, [(File, [Int])])]
parse' [] = []
parse' (l:list) 
    | isWordLine l = (fst word_pair, (snd word_pair) ++ (map parseFileLine (nextFilesLine list)) ) : parse' list
    | otherwise = parse' list
    where
        word_pair = parseWordLine l
    
isWordLine :: String -> Bool
isWordLine [] = False
isWordLine (c:_)
    | not (isSpace c)  = True
    | otherwise = False
    
nextFilesLine :: [String] -> [String]
nextFilesLine [] = []
nextFilesLine (l:list) 
    | isFileLine l = l : (nextFilesLine list)
    | otherwise = []
    
isFileLine :: String -> Bool
isFileLine [] = False
isFileLine (c:_) 
    | isSpace c = True
    | otherwise = False
    
parseWordLine :: Zeile -> (Wort, [(File, [Int])]) 
parseWordLine z = (fst wort_pair, [(fst file_pair, zeilen)])
    where 
        wort_pair = parseWort z
        file_pair = parseFile (snd wort_pair)
        zeilen = parseLines (snd file_pair)

parseFileLine :: Zeile -> (File, [Int]) 
parseFileLine z = (fst file_pair, zeilen)
   where
       file_pair = parseFile z
       zeilen = parseLines (snd file_pair)
    
parseWort :: Zeile -> (Wort,Zeile)
parseWort s = (head (words s), unwords (tail (words s)))
    
parseFile :: Zeile -> (File, Zeile)
parseFile f = (head (words f), unwords (tail (words f)))

parseLines :: Zeile -> [Int]
parseLines l = [read w :: Int | w <- (words l)]