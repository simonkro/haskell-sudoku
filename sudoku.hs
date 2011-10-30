import Data.Char
import Data.List
import Data.Array
import Data.Maybe

-- can't believe there is no build in version of this
groupsOf :: (Integral t) => t -> [a] -> [[a]] 
groupsOf _ [] = []
groupsOf n xs = genericTake n xs : groupsOf n (genericDrop n xs)  

neighbours = map (\i -> filter (neighbour i) [0..80]) [0..80]
  where neighbour i j = (i /= j) && (box i == box j || row i == row j || col i == col j)
        box n = n `div` 27 * 3 + n `mod` 9 `div` 3
        row n = n `div` 9
        col n = n `mod` 9

solve game index = fmap fromJust $ find isJust $ map try [1..9]
  where 
    try t
        | f /= 0 && f /= t = Nothing
        | taken            = Nothing
        | index == 80      = Just game'
        | otherwise        = solve game' (index+1)
      where
        f = game ! index
        game' = game // [(index, t)]
        taken = elem t $ map (game!) $ neighbours !! index

parse = foldr toNumber []
  where toNumber d acc
          | elem d ['1'..'9'] = digitToInt d:acc
          | d == '_'          = 0:acc
          | otherwise         = acc

readGame xs = do
  l <- getLine
  if null l then return xs else readGame $ xs ++ l

main = do
  string <- readGame ""
  let game = listArray (0, 80) $ parse string
  let solved = elems . fromJust $ solve game 0
  let lines = groupsOf 18 (intersperse ' ' (map intToDigit solved))

  putStrLn $ unlines lines
  return ()
