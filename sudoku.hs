import Data.Char
import Data.List
import Data.Array
import Data.Maybe

{-
neighbourhood i = filter (neighbours i) [0..80]
  where neighbours i j = box i == box j || row i == row j || col i == col j
        box n = n `div` 27 * 3 + n `mod` 9 `div` 3
        row n = n `div` 9
        col n = n `mod` 9
-}

-- can't believe there is no build in version of this
groupsOf :: (Integral t) => t -> [a] -> [[a]] 
groupsOf _ [] = []
groupsOf n xs = genericTake n xs : groupsOf n (genericDrop n xs)  

row = [[x*9..x*9+8] | x <- [0..8]]
col = [[x,x+9..x+72] | x <- [0..8]]
box = [map (y*27+x*3+) [0, 1, 2, 9, 10, 11, 18, 19, 20] | x <- [0..2], y <- [0..2]]

solveGame g index try
  | index > 80         = Just g
  | try > 9            = Nothing
  | g ! index /= 0     = solveGame g (index+1) 1
  | not (validGame g') = solveGame g index (try+1)
  | isNothing solved   = solveGame g index (try+1)
  | otherwise          = solved
  where g' = g // [(index, try)]
        solved = solveGame g' (index+1) 1

validGame :: Array Int Int -> Bool
validGame g = all valid (row ++ col ++ box)
  where valid = all (==1) . map length . groups
        groups = group . sort . filter (>0) . map (g!)

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
  let game = listArray (0, 80) $ parseGame s
  let solved = elems . fromJust $ solveGame game 0 1
  let lines = groupsOf 18 (intersperse ' ' (map intToDigit solved))

  putStrLn $ unlines lines
  return ()
