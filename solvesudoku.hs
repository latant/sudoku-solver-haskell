import Data.List


main = interact solveSudoku

solveSudoku :: String -> String
solveSudoku = resultString . solve . lines

type Table = [[Char]]
at table i j = (table !! i) !! j
data SudokuResult = Solved Table | Unsolved Table | Unsolvable Table Int Int
abc = ['1'..'9']
(width, height) = (9, 9)
(regW, regH) = (3, 3)
regRowCount = height `div` regH
regColumnCount = width `div` regW

resultString :: SudokuResult -> String
resultString (Solved table) = "Solved table:\n" ++ (unlines table)
resultString (Unsolved table) = "Unsolved table:\n" ++ (unlines table)
resultString (Unsolvable table row col) =
  "Unsolvable table! No good solution at field "
   ++ (show col) ++ " in row " ++ (show row) ++ ":\n" ++ (unlines table)

solve :: Table -> SudokuResult
solve rows
  | all (all (\e -> e `elem` abc)) rows = Solved rows
  | otherwise = Unsolved rows




possibleAt :: Table -> Int -> Int -> [Char]
possibleAt table i j
  | elem e abc = [e]
  | otherwise = abc \\ excluding
  where
    e = at table i j
    excluding = (rowValues `union` columnValues) `union` regionValues
    rowValues = table !! i
    columnValues = map (\row -> row !! j) table
    regionValues = foldl1 (++) $ subTable i0 (i0 + regH) j0 (j0 + regW) table
    (i0, j0) = (i `div` regH, j `div` regW)

replace :: [[a]] -> Int -> Int -> a -> [[a]]
replace [] _ _ _ = []
replace (as:ass) i j e
  | i == 0 = (replaceAt as j e) : ass
  | otherwise = as : (replace ass (i - 1) j e)
replaceAt :: [a] -> Int -> a -> [a]
replaceAt [] _ _ = []
replaceAt (a:as) i e
   | i == 0 = e : as
   | otherwise = a : (replaceAt as (i - 1) e)

subTable i0 i1 j0 j1 list = map (subList j0 j1) $ subList i0 i1 list
subList i0 i1 list = take (i1 - i0) $ drop i0 list
