import Data.Char
import Data.List
import Data.Array
import Data.Maybe

-- can't believe there is no build in version of this
groupsOf :: (Integral t) => t -> [a] -> [[a]] 
groupsOf _ [] = []
groupsOf n xs = genericTake n xs : groupsOf n (genericDrop n xs)  

neighbours = map (\i -> filter (neighbour i) [0..80]) [0..80]
  where neighbour i j = box i == box j || row i == row j || col i == col j
        box n = n `div` 27 * 3 + n `mod` 9 `div` 3
        row n = n `div` 9
        col n = n `mod` 9

solveGame g index try
  | index > 80         = Just g
  | try > 9            = Nothing
  | g ! index /= 0     = solveGame g (index+1) 1
  | taken              = solveGame g index (try+1)
  | isNothing solved   = solveGame g index (try+1)
  | otherwise          = solved
  where g' = g // [(index, try)]
        solved = solveGame g' (index+1) 1
        taken = elem try $ map (g!) $ neighbours !! index

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
