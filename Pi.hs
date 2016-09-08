import UCT

eval coeffs = 1 / abs (pi - numify coeffs)

-- | numify [3, 1, 2] = 3.12
numify coeffs = sum [c / 10^i | (c, i) <- zip coeffs [0..]]

problem coeffs
  | length coeffs > 10 = []
  | otherwise = [(coeffs ++ [i], eval (coeffs ++ [i])) | i <- [0..9]]

main :: IO ()
main = do
  let trees = take 100 $ iterate (fst . search problem) (emptyTree problem [])
  mapM_ (putStrLn . show . bestState byVisits) $ trees

