import System.Environment   
import Data.List  
  
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

main = do  
   args <- getArgs  
   progName <- getProgName  
   --putStrLn "The arguments are:"  
   --mapM putStrLn args  
   --putStrLn "The program name is:"  
   --putStrLn progName
   let tupels = map makeTupel args -- list with tupels
   let tupels_cleanded = map removeEqual tupels
   mapM print tupels_cleanded