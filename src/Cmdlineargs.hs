module Cmdlineargs 
( getOption, getInputFiles, getOutputFile, isValidOption ) where

import System.Environment(getArgs)
import System.IO  

type Option = String
type Value = String
type Valid = Bool

-- this function check the number of given parameters
checkArgs :: [String] -> Bool
checkArgs [] = False 
checkArgs (x:xs) = True

--check the string for - charakter at the front
isValidOption :: String -> Bool
isValidOption str 
   | str == "-p" = True
   | str == "-i" = True
   | str == "-c" = True
   | str == "-q" = True
   | str == "-s" = True
   | str == "-t" = True 
   | otherwise = False
   
getOption :: [String] -> [(Option, Value)]
getOption [] = [] 
getOption args = removeEqSign (getOption' (getOnlyOptions args))
 
-- separate strings and create a list of (String,String) Pair
getOption' :: [String] -> [(Option, Value)]
getOption' list = [break (=='=') e | e <- list]

getOnlyOptions :: [String] -> [String]
getOnlyOptions list = filter (\ a -> (head a) == '-') list

removeEqSign :: [(Option, Value)] -> [(Option, Value)]
removeEqSign list = [(fst a, filter (\ a -> a /= '=') (snd a))  | a <- list] 

--sortOption :: [String] -> [String]
--sortOption list = [if (isOption e) then e else "" | e <-list] 
                
--isValidOption :: [(Option,Value)] -> [(Option,Value)]
--isValidOption list = [if isOption (fst e) then ((fst e), (snd e)) else ("","") | e <-list]

--deleteIfEmpty :: [(Option,Value)] -> [(Option,Value)]
--deleteIfEmpty pairs = [ | e <- pairs]

-- filter (\ a -> (head a) /= '-') ["-p, ....]
-- head (filter (\ a -> (head a) /= '-') ["-p, ....]

getInputFiles :: [String] -> [String]
getInputFiles [] = []
getInputFiles list = 
    if (getNonOptions list) /= []
        then tail (getNonOptions list)
        else []
        
getNonOptions :: [String] -> [String]
getNonOptions [] = []
getNonOptions list = filter (\ a -> (head a) /= '-') list

getOutputFile :: [String] -> String
getOutputFile [] = []
getOutputFile list = 
    if (getNonOptions list) /= []
        then head (getNonOptions list)
        else []


main = do
    -- Read given commandline arguments
    args <- getArgs
    print args
    -- Check args for correctness
    --let isitaoption = isOption (args !! 0)
    --if isitaoption then print "Is an option --> Must be added to data structure" else print "Is not an option"

   -- let options = sortOption args
   -- let pairOption = getOption args
   -- let onlyValidOptions = isValidOption pairOption
   -- print onlyValidOptions
    let listOptions = getOnlyOptions args
    let pairOptions = getOption listOptions



    print pairOptions
    let inputFiles = getInputFiles args
    print inputFiles

    let outputFile = getOutputFile args
    print outputFile


