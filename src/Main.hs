import System.Environment   
import Data.List    
import System.Directory  
import System.IO   
  
import Index  

type Text = String
type Wort = String
type File = String
 
-- get a list of text and a list of files and make a list of pairs
makePair :: [Text] -> [File] -> [(Text,File)]
makePair text files = zip text files

-- function for argument -c
printWordNumber :: [(Wort, [(File, [Int])])] -> IO ()
printWordNumber index = print (length (map fst index))

-- function for argument -p
printIndex :: [(Wort, [(File, [Int])])] -> IO ()
printIndex index = putStr (printAsString index)

-- function for argument -i
createIndex :: [(Text,File)] -> [(Wort, [(File, [Int])])]
createIndex content = index content

-- function for argument -q
printIndexForWord :: Wort -> [(Wort, [(File, [Int])])] -> IO ()
printIndexForWord word index
    | length elements > 0 = putStr (printAsString elements)
    | otherwise = putStrLn ("no elements for word: '" ++ word ++ "' in list!")
    where elements = (filter (\ a -> (fst a) == word) index)

-- function for argument -t
printIndexForFile :: File -> [(Wort, [(File, [Int])])] -> IO ()
printIndexForFile file index
    | length elements > 0 = putStr (printAsString elements)
    | otherwise = putStrLn ("no elements for file: '" ++ file ++ "' in list!")
    where elements = (filter (\ a -> elem file (map fst (snd a)) ) index)

-- function for argument -s
printIndexForWordPartial :: String -> [(Wort, [(File, [Int])])] -> IO ()
printIndexForWordPartial partialWord list
    | length elements > 0 = putStr (printAsString elements)
    | otherwise = putStrLn ("no elements for string: '" ++ partialWord ++ "' in list!")
    where elements = (filter (\ a -> isPartOfWord partialWord (fst a)) list)

isPartOfWord :: String -> Wort -> Bool
isPartOfWord [] [] = True
isPartOfWord [] _ = True
isPartOfWord _ [] = False
isPartOfWord (s:str) (w:word)
    | s == w = True && isPartOfWord str word
    | otherwise = False

-- function to write index in file
writeOutInFile :: File -> String -> IO ()
writeOutInFile filepath content = writeFile filepath content 

main = do
    -- list with files
    let files = ["Testdaten/Euler.txt"]
    
    -- read several files
    content_list <- mapM readFile files
    
    let content = makePair content_list files 
    let idx = createIndex content
    
    printIndex idx
    
    --printWordNumber idx
    
    --putStr (printAsString idx)
    
    --printIndexForWord "mo" idx
    
    --printIndexForFile "text1.txt" idx
    
    --printIndexForWordPartial "m" idx