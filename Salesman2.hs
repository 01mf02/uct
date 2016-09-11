import Control.Monad (foldM_)
import Data.Array
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)
import System.Random (setStdGen, mkStdGen)
import Debug.Trace
import UCT2

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

process l = zipWith (\ i x -> (x, 1 / i ** 6)) [1 ..] $ map fst $ sortOn snd l

stateActs dists n visited
  | lv  < n = [(c : visited, score c) | c <- [1..n], c `notElem` visited]
  | lv == n = [(1 : visited, score 1)]
  | otherwise = []
  where
    score c = dists ! (head visited, c)
    lv = length visited

tspReward dists s =
  let d = traceShowId $ tripDistance dists s
  in 33000 / d

tripDistance dists visited =
  sum $ map (dists !) $ zip visited (tail visited)

-- According to: http://elib.zib.de/pub/mp-testdata/tsp/tsplib/tsp/att48.opt.tour
bestSolution =
  [1,8,38,31,44,18,7,28,6,37,19,27,17,43,30,36,46,33,20,47,21,32,39,48,5,42,24,10,45,35,4,26,2,29,34,41,16,22,3,23,14,25,13,11,12,15,40,9,1]

main = do
  setStdGen (mkStdGen 0)
  cities <- readCityFile <$> readFile "att48.tsp"
  let dists = distances cities
  let maxDist = maximum $ elems dists
  --print $ eval dists bestSolution
  let tsp = Problem {successors = process . stateActs dists (length cities), reward = tspReward dists}
  foldM_ (\ v _ -> treePolicy tsp v) (emptyTree tsp [1]) [1 .. 15000]
