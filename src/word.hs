-- Test if first char is valid
isFirstCharValid :: Char -> Bool
isFirstCharValid x
    | x == '_' = True
    | x >= 'A' && x <= 'Z' = True
    | x >= 'a' && x <= 'z' = True
    | x == 'ä' || x == 'Ä' = True
    | x == 'ö' || x == 'Ö' = True
    | x == 'ü' || x == 'Ü' = True
    | otherwise = False
    
-- Test if any char is valid
isAnyCharValid :: Char -> Bool
isAnyCharValid c
    | (isFirstCharValid c || c == '-' || (c >= '0' && c <= '9')) = True
    | otherwise = False

-- Test if the word is valid
isWordValid :: String -> Bool
isWordValid [] = False
isWordValid (x:[]) = isAnyCharValid x
isWordValid (x:xs) = isAnyCharValid x && isWordValid xs