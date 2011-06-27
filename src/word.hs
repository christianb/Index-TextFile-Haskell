-- Test if the word is valid
isWordValid :: String -> Bool
isWordValid [] = False
isWordValid (x:[]) = isHeadValid x
isWordValid (x:xs) = isHeadValid x && isTailValid xs

-- Tests if the head of a word is valid
isHeadValid :: Char -> Bool
isHeadValid x
    | x == '_' = True
    | x >= 'A' && x <= 'Z' = True
    | x >= 'a' && x <= 'z' = True
    | x == 'ä' || x == 'Ä' = True
    | x == 'ö' || x == 'Ö' = True
    | x == 'ü' || x == 'Ü' = True
    | otherwise = False

isTailValid :: String -> Bool
isTailValid [] = False
isTailValid (c:[])
    | (isHeadValid c || c == '-' || (c >= '0' && c <= '9')) = True
    | otherwise = False
isTailValid (c:cs)
    | (isHeadValid c || c == '-' || (c >= '0' && c <= '9')) = (True && isTailValid cs)
    | otherwise = False