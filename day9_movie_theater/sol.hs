type Tile = (Integer, Integer)
type Rectangle = (Tile, Tile)

area :: Rectangle -> Integer
area rect = dx * dy
 where
  (x1, y1) = fst rect
  (x2, y2) = snd rect
  dx = abs $ x2 - x1 + 1
  dy = abs $ y2 - y1 + 1

combine :: [Tile] -> [Rectangle]
combine tiles = [(t1, t2) | t1 <- tiles, t2 <- tiles, t1 /= t2]

parseTile :: String -> Tile
parseTile str = (read x, read (drop 1 rest))
 where
  (x, rest) = break (== ',') str 

main :: IO ()
main = do
 content <- readFile "input.txt"
 let fileLines = lines content
 let tiles = map parseTile fileLines
 let result = maximum $ map area (combine tiles)
 print result
