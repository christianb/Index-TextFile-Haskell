import System.Environment   
import Data.List    
import System.Directory  
import System.IO

type Text = String
type Zeile = String
type Wort = String

-- trennt text in zeilen auf
split :: Text -> [Zeile]
split t = lines t

-- Test if the head of a word is valid
isHeadChar :: Char -> Bool
isHeadChar x
    | x == '_' = True
    | x >= 'A' && x <= 'Z' = True
    | x >= 'a' && x <= 'z' = True
    | x == 'ä' || x == 'Ä' = True
    | x == 'ö' || x == 'Ö' = True
    | x == 'ü' || x == 'Ü' = True
    | x == ' ' = True
    | otherwise = False

-- Ttest if the Head of a Word is valid
isAnyCharValid :: Char -> Bool
isAnyCharValid c = isHeadChar c || c == '-' || (c >= '0' && c <= '9')

replaceCharWithSpace :: Char -> Char
replaceCharWithSpace c = ' '

ignore :: [Zeile] -> [Zeile]
ignore [] = []
ignore (z:zs) =
    let ignoreAny e
        ignoreHead e = filter (\ a-> isHeadChar a) e
    in ignoreHead (ignoreAny z) : ignore zs
