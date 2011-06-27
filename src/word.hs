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

-- Test if the tail of a word is valid
isTailValid :: String -> Bool
isTailValid [] = True
isTailValid (c:cs)
    | isAnyCharValid = (True && isTailValid cs)
    | otherwise = False
    where isAnyCharValid = isHeadValid c || c == '-' || (c >= '0' && c <= '9')
    
-- removes only words those pattern is correct
cleanList :: [String] -> [String]
cleanList cs = [word | word <- cs, isWordValid word]