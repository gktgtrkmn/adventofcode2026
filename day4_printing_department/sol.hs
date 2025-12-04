import qualified Data.Map as M
import Data.Map (keys)

type Point = (Int, Int)
type TileMap = M.Map Point Char
type Border = (Int, Int)

readTile :: [String] -> (Border, TileMap)
readTile rows = ((length rows, length $ head rows),
  M.fromList [((x, y), c) | (x, row) <- zip [0..] rows, (y, c) <- zip [0..] row]
  )

count :: Eq a => a -> [a] -> Int
count x = length . filter (x==)

checkAccess :: TileMap -> Border -> Point -> Int -> Bool
checkAccess mp border p limit = charValid && adjacentValid
  where
    charValid = mp M.! p == '@'
    adjacentValid = count '@' (map (mp M.!) validNeighbors) < limit
    (px, py) = p
    (maxX, maxY) = border
    inside (x, y) = x >= 0 && x < maxX && y >= 0 && y < maxY
    candidates = [(x, y) | x <- [px-1..px+1], y <- [py-1..py+1], (x, y) /= p]
    validNeighbors = filter inside candidates

part1 :: TileMap -> Border -> Int
part1 mp border = length validPoints
 where 
  points = M.keys mp
  validPoints = filter (\p -> checkAccess mp border p limit) points
  limit = 4

main :: IO ()
main = do
  content <- readFile "input.txt"
  let fileLines = lines content
  let (border, tilemap) = readTile fileLines
  print $ part1 tilemap border
  