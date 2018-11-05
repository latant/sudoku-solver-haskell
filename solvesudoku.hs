import Data.List

main = interact (result . solve . toTable)

data SudokuResult = Solved [Char] | Unsolvable [Char] (Int, Int)
result (Solved table) = "Solved table:\n" ++ (toStr table)
result (Unsolvable table (i, j)) =
  "Unsolvable table! Conflict at (" ++ (show i) ++ "," ++ (show j) ++ ").\n"
    ++ (toStr table)

toTable chars = chars `intersect` (abc `union` emp)
toStr table = unlines $ map (intersperse ' ') $ nGroup size table

(regW, regH) = (3, 3)
size = regW * regH
fieldN = size * size
abc = take size $ ['1'..'9'] ++ ['a'..]
emp = ['0','-']
toIJ index = (index `div` size, index `mod` size)

solve table = case fieldToSolve table of
  Nothing -> Solved table
  Just (i, p) -> case length p of
    0 -> Unsolvable table $ toIJ i
    1 -> solve $ replaceAt i (p !! 0) table
    otherwise -> branch (i, p) table
    where branch (i, p) = try (i, p) 0
          try (i, p) j table
            | j == length p = Unsolvable table $ toIJ i
            | otherwise = res $ solve $ replaceAt i (p !! j) table
            where res (Unsolvable _ _) = try (i, p) (j + 1) table
                  res r = r

fieldToSolve table = maybe Nothing try $ unfat 0
  where
    try (i, p) = tryOn (i, p) (unfat $ i + 1)
    tryOn f Nothing = Just f
    tryOn (i, p) (Just (i1, p1))
      | length p == 1 = Just (i, p)
      | length p <= length p1 = tryOn (i, p) (unfat $ i1 + 1)
      | length p1 == 1 = Just (i1, p1)
      | otherwise = tryOn (i1, p1) (unfat $ i1 + 1)
    unfat i
      | i >= fieldN = Nothing
      | (table !! i) `elem` abc = unfat (i + 1)
      | otherwise = Just (i, possibleAt i table)

possibleAt index table = abc \\ (rowVals `union` colVals `union` regVals)
  where
    rowVals = values [i] [0..size-1]
    colVals = values [0..size-1] [j]
    regVals = values [ri0..ri1] [rj0..rj1]
    (ri0, rj0) = (i - i `mod` regH, j - j `mod` regW)
    (ri1, rj1) = (ri0 + regH - 1, rj0 + regW - 1)
    values list0 list1 = map valueAt $ [(i,j) | i <- list0, j <- list1]
    valueAt (i, j) = table !! (i * size + j)
    (i, j) = toIJ index

replaceAt _ _ [] = []
replaceAt i e (a:as)
   | i == 0 = e : as
   | otherwise = a : (replaceAt (i - 1) e as)
nGroup _ [] = []
nGroup n list = group : (nGroup n rest)
  where (group, rest) = splitAt n list
