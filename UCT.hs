module UCT where

import Data.List (sortOn)
import Data.Ord (Down(..))
import Debug.Trace

type Reward = Double
type Problem s = s -> [(s, Reward)]

data Tree s = Tree {
  state :: s
, visits :: Int
, rewards :: Reward
, children :: [(Tree s, Reward)]
, actions :: [(s, Reward)]
} deriving Show

emptyTree :: Problem s -> s -> Tree s
emptyTree problem s = Tree {
  state = s
, visits = 0
, rewards = 0
, children = []
, actions = sortOn (Down . snd) (problem s)
}

avgReward :: Tree s -> Reward
avgReward tree = rewards tree / fromIntegral (visits tree)

childScore :: Tree s -> Tree s -> Double
childScore parent child = avgReward child + c_p * sqrt (log t / s)
  where c_p = 0
        t = fromIntegral (visits parent)
        s = fromIntegral (visits child)

search :: Problem s -> Tree s -> Tree s
search p tree = case actions tree of
  (act, rew) : acts -> update p (emptyTree p act, rew) (tree {actions = acts})
  [] -> case sortOn (Down . childScore tree . fst) (children tree) of
    [] -> tree {visits = visits tree + 1}
    best : rest -> update p best (tree {children = rest})

update :: Problem s -> (Tree s, Reward) -> Tree s -> Tree s
update p (child, rew) parent = parent
  { rewards = rewards parent + rew + gamma * (rewards child' - rewards child)
  , visits = visits parent + 1
  , children = (child', rew) : children parent
  }
  where gamma = 1
        child' = search p child

bestState :: Ord a => (Tree s -> a) -> Tree s -> s
bestState f tree = case sortOn (f . fst) (children tree) of
  (best, _) : _ -> bestState f best
  [] -> state tree

byReward :: Tree s -> Down Reward
byReward = Down . avgReward

byVisits :: Tree s -> Down Int
byVisits = Down . visits

normRewards l = map (\ (s, r) -> (s, (r - min) / (max - min))) l
  where max = maximum (map snd l)
        min = minimum (map snd l)

