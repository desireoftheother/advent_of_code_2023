import System.IO
import Data.Char (isDigit, digitToInt)
import Data.List (minimumBy)
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

splitLineBySpelling :: String -> String -> (String, [String])
splitLineBySpelling line spelling = (spelling, splitOn spelling line)

createSpellingsMap :: String -> Map.Map String Int -> [(String, [String])]
createSpellingsMap line spellingMapping = map (splitLineBySpelling line) (Map.keys spellingMapping)

main = do
    withFile "input.txt" ReadMode (\fileHandler -> do
        contents <- hGetContents fileHandler
        let splittedLines = lines contents
        print "2"
        )
