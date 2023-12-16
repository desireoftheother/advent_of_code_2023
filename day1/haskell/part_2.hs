import System.IO
import Data.Char
import Consts (spellingToDigitsMapping)

getTensAndOnes :: String -> Int
getTensAndOnes line = 10 * digitToInt tens  + digitToInt ones
    where filteredLine = filter isDigit line
          tens = head filteredLine
          ones = last filteredLine

calculateCallibrationValues :: [String] -> Int
calculateCallibrationValues = sum . map getTensAndOnes

main = do
    withFile "input.txt" ReadMode (\fileHandler -> do
        contents <- hGetContents fileHandler
        let splittedLines = lines contents
        print $ calculateCallibrationValues $ splittedLines
        )
