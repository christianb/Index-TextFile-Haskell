import System.Environment   
import Data.List    
import System.Directory  
import System.IO   
  
replaceEqualWithSpace :: String -> String
replaceEqualWithSpace [] = []
replaceEqualWithSpace (w:ws) = 
    if w /= '='
        then w:(replaceEqualWithSpace ws)
        else replaceEqualWithSpace ws

removeEqual :: (String,String) -> (String,String)
removeEqual pair = 
    let h = fst pair
        s = snd pair
    in (h,replaceEqualWithSpace s)
        
makeTupel :: String -> (String, String)
makeTupel w = span ('='/=) w

p :: IO ()
p = print "call method for -p"

t :: IO ()
t = print "call method for -t"

dispatch :: [(String, IO ())]  
dispatch =  [ ("-p", p), ("-t", t)]

getMyArgs :: [(String,String)] -> [String]
getMyArgs [] = []
getMyArgs (p:ps) = (fst p):(getMyArgs ps)

{-
main = do  
   args <- getArgs  
   progName <- getProgName  
   --putStrLn "The arguments are:"  
   --mapM putStrLn args  
   --putStrLn "The program name is:"  
   --putStrLn progName
   let tupels = map makeTupel args -- list with tupels
   let tupels_cleaned = map removeEqual tupels -- list with tupels without equall sign
   mapM print tupels_cleaned
   let only_args = getMyArgs tupels_cleaned
   map only_args
   --let (command:args) = tupels_cleaned
   let (Just action) = lookup "-p" dispatch  
   action-}