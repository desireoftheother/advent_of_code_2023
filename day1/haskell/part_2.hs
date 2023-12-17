import System.IO
import Data.Char (isDigit, digitToInt)
import Data.List (isInfixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map

spellingToDigitsMapping :: Map.Map String Int
spellingToDigitsMapping = Map.fromList [
    ("one", 1),
    ("two", 2),
    ("three", 3),
    ("four", 4),
    ("five", 5),
    ("six", 6),
    ("seven", 7),
    ("eight", 8),
    ("nine", 9),
    ("1", 1),
    ("2", 2),
    ("3", 3),
    ("4", 4),
    ("5", 5),
    ("6", 6),
    ("7", 7),
    ("8", 8),
    ("9", 9)]

data Digit = Tens | Ones deriving (Show)

checkIfSpellingPresent :: String -> String -> (String, Bool)
checkIfSpellingPresent line spelling = (spelling, isInfixOf spelling line)

createSpellingsMap :: String -> Map.Map String Int -> [(String, Bool)]
createSpellingsMap line spellingMapping = map ifSpellingPresent keys
    where keys = Map.keys spellingMapping
          ifSpellingPresent = checkIfSpellingPresent line

ifSpellingPresent :: (String, Bool) -> Bool
ifSpellingPresent (_, flag) = flag

searchSpellingsIteratively :: String -> Int -> Map.Map String Int -> Digit -> String
searchSpellingsIteratively line windowSize spellingMapping digit
    | any ifSpellingPresent spellingPresence = spellingLookedUp
    | otherwise = searchSpellingsIteratively line (windowSize + 1) spellingMapping digit
     where spellingPresence = createSpellingsMap subs spellingToDigitsMapping
           spellingLookedUp = fst $ head $ filter ifSpellingPresent $ spellingPresence
           subs = case digit of 
            Tens -> take windowSize line
            Ones -> drop (length line - windowSize) line

getCalibrationValues :: String -> Int
getCalibrationValues line = 10*tensProcessed + onesProcessed
    where searchSpelling = searchSpellingsIteratively line 1 spellingToDigitsMapping  
          tens = Map.lookup (searchSpelling Tens) spellingToDigitsMapping
          ones = Map.lookup (searchSpelling Ones) spellingToDigitsMapping
          tensProcessed = case tens of 
            Just x -> x
            Nothing -> 0
          onesProcessed = case ones of
            Just x -> x
            Nothing -> 0


main = do
    withFile "input.txt" ReadMode (\fileHandler -> do
        contents <- hGetContents fileHandler
        let splittedLines = lines contents
        let processedLines = map getCalibrationValues splittedLines
        let s = sum processedLines
        print s
        )
