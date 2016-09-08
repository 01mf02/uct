import Control.Arrow
import Data.Array
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import UCT

readCityLine :: [String] -> Maybe (Int, Int, Int)
readCityLine [i, x, y] = do
  i' <- readMaybe i
  x' <- readMaybe x
  y' <- readMaybe y
  return (i', x', y')
readCityLine _ = Nothing

readCityFile = mapMaybe (readCityLine . words) . lines

distances cities = array ((1, 1), (48, 48)) $
  concatMap (\ (i1, x1, y1) ->
    map (\ (i2, x2, y2) ->
      ((i1, i2), sqrt ((fI x1 - fI x2)^2 + (fI y1 - fI y2)^2))
    ) cities) cities
  where fI = fromIntegral

stateActs dists n visited
  | lv  < n = [(c : visited, score c) | c <- [1..n], c `notElem` visited]
  | lv == n = [(1 : visited, score 1)]
  | otherwise = []
  where
    score c = dists (head visited) c
    lv = length visited

tripDistance dists visited =
  sum $ map (dists !) $ zip visited (tail visited)

-- According to: http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/att48.opt.tour
bestSolution =
  [1,8,38,31,44,18,7,28,6,37,19,27,17,43,30,36,46,33,20,47,21,32,39,48,5,42,24,10,45,35,4,26,2,29,34,41,16,22,3,23,14,25,13,11,12,15,40,9,1]

main = do
  cities <- readCityFile <$> readFile "att48.tsp"
  let dists = distances cities
  let maxDist = maximum $ elems dists
  let distf c1 c2 = 1 - (dists ! (c1, c2)) / maxDist
  --print $ eval dists bestSolution
  let tspProblem = stateActs distf (length cities)
  let trees = take 15000 $ iterate (search tspProblem) $ emptyTree tspProblem [1]
  mapM_ (print . (tripDistance dists {- &&& id-}) . bestState byReward) trees
