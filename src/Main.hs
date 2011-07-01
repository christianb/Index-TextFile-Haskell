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
printIndex index = print' index

-- function for argument -i
createIndex :: [(Text,File)] -> [(Wort, [(File, [Int])])]
createIndex content = index content

main = do
    -- list with files
    let files = ["text1.txt", "text2.txt"]
    
    -- read several files
    content_list <- mapM readFile files
    
    let content = makePair content_list files 
    let idx = createIndex content
    
    printIndex idx
    
    printWordNumber idx