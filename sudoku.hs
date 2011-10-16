import Data.Char
import Data.List

{-
neighbourhood i = 
  filter (sameNeighbourhood i) [0..80]
  where
  sameNeighbourhood i j =
    row i == row j || col i == col j || box i == box j
    where row x = x `div` 9
          col x = x `mod` 9
          box x = x `div` 27 * 3 + x `mod` 9 `div` 3
-}

-- can't believe there are no build in versions of this
valuesAt :: (Integral t) => [a] -> [t] -> [a]
valuesAt _ [] = [] 
valuesAt xs (y:ys) = (genericIndex xs y):valuesAt xs ys

replaceAt :: (Integral t) => [a] -> t -> a -> [a]
replaceAt xs n a = (genericTake n xs) ++ [a] ++ (genericDrop (n+1) xs)

groupsOf :: (Integral t) => t -> [a] -> [[a]] 
groupsOf _ [] = []
groupsOf n xs = genericTake n xs : groupsOf n (genericDrop n xs)  

--

row = [[x*9..x*9+8] | x <- [0..8]]
col = [[x,x+9..x+72] | x <- [0..8]]
box = [map (y*27+x*3+) [0, 1, 2, 9, 10, 11, 18, 19, 20] | x <- [0..2], y <- [0..2]]

solveGame g index try
  | index > 80         = g
  | try > 9            = []
  | g !! index /= 0    = solveGame g (index+1) 1
  | not (validGame g') = solveGame g index (try+1)
  | solved == []       = solveGame g index (try+1)
  | otherwise          = solved
  where g' = replaceAt g index try
        solved = solveGame g' (index+1) 1

validGame :: [Int] -> Bool
validGame g = all valid (row ++ col ++ box)
  where valid = all (==1) . map length . groups
        groups = group . sort . filter (>0) . valuesAt g

parseGame :: String -> [Int]
parseGame = foldr toNumber []
  where toNumber d acc
          | elem d ['1'..'9'] = digitToInt d:acc
          | d == '_'          = 0:acc
          | otherwise         = acc

readGame :: String -> IO String
readGame xs = do
  l <- getLine
  if null l then return xs else readGame $ xs ++ l

main = do
  s <- readGame ""
  let solved = solveGame (parseGame s) 0 1
  let lines = groupsOf 18 (intersperse ' ' (map intToDigit solved))

  putStrLn (intercalate "\n" lines)
  return ()
