module UCT2 where

import Data.List (break, sortOn)
import Data.Ord (Down(..))
import System.Random (randomRIO)
import Debug.Trace

type Probability = Double
type Reward = Double

data Problem s = Problem {
  successors :: s -> [(s, Probability)]
, reward :: s -> Reward
}

data Tree s = Tree {
  state :: s
, visits :: Int
, rewards :: Reward
, embryos :: [(s, Probability)]
, children :: [Tree s]
} deriving Show

emptyTree problem s = Tree {
  state = s
, visits = 0
, rewards = 0
, embryos = successors problem s
, children = []
}

avgReward tree = rewards tree / fromIntegral (visits tree)

childScore parent child = avgReward child + c_p * sqrt (log t / s)
  where c_p = 0
        t = fromIntegral (visits parent)
        s = fromIntegral (visits child)

visit v rew = v {visits = visits v + 1, rewards = rewards v + rew}

cdf l = foldl (\ (acc, sum) (x, w) ->
  ((x, (sum, w, sum + w)) : acc, sum + w)) ([], 0) l

cdfSample l = do
  let (cl, lim) = cdf l
  r <- randomRIO (0, lim)
  return (cl, \ (x, (min, _, max)) -> min <= r && r <= max)

weightedSample l = do
  (cl, f) <- cdfSample l
  let (no, yes) = break f cl
  return (head yes, map (\ (x, (_, w, _)) -> (x, w)) $ tail yes ++ no)

defaultPolicy p s = case successors p s of
  [] -> return (reward p s)
  xs -> weightedSample xs >>= defaultPolicy p . fst . fst

treePolicy p v
  | embryos v == [] = case sortOn (Down . childScore v) (children v) of
      [] -> return $ visit v (reward p (state v))
      best : rest -> do
        best' <- treePolicy p best
        let v' = visit v (rewards best' - rewards best)
        return $ v' {children = best' : rest}
  | otherwise = do
      ((chosen, _), rest) <- weightedSample (embryos v)
      let baby = emptyTree p chosen
      rew <- defaultPolicy p chosen
      let v' = visit v rew
      return $ v' {children = visit baby rew : children v, embryos = rest}
