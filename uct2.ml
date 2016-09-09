type reward_t = float
type probability = float

type 's problem = {
  successors : 's -> ('s * probability) list
; reward : 's -> reward_t
}

type 's tree = {
  state : 's
; visits : int
; rewards : reward_t
; children : 's tree list
; actions : 's list
}

let empty_tree p s = {
  state = s
; visits = 0
; rewards = 0.
; children = []
; actions = List.map fst (List.sort (fun (_, x) (_, y) -> compare y x) (p.successors s))
}

let avg_reward tree = tree.rewards /. float_of_int tree.visits

let child_score parent child =
  let c_p = sqrt 2. and fi = float_of_int in
  avg_reward child +. c_p *. sqrt (log (fi parent.visits) /. fi child.visits)

let child_cmp parent c1 c2 =
  compare (child_score parent c2) (child_score parent c1)

(* cdf [("a", 0.1); ("b", 0.9)] = ([("b", (0.1, 1.)); ("a", (0., 0.1))], 1.) *)
let cdf l = List.fold_left (fun (acc, sum) (x, w) ->
  let sum' = sum +. w in ((x, (sum, sum')) :: acc, sum')) ([], 0.0) l

let weighted_sample = function
  [] -> failwith "weighted_sample: empty list"
| xs ->
    let (xs', lim) = cdf xs in
    let r = Random.float lim in
    fst (List.find (fun (x, (min, max)) -> min <= r && r <= max) xs')

let uniform_sample xs =
  let r = Random.int (List.length xs) in fst (List.nth xs r)

let best_sample xs = fst (List.hd (List.sort (fun (_, x) (_, y) -> compare y x) xs))

let rec default_policy p s =
  match (p.successors) s with
    [] -> p.reward s
  | xs -> default_policy p (weighted_sample xs)

let backup v rew =
  {v with visits = v.visits + 1; rewards = v.rewards +. rew}

let rec tree_policy p v = match v.actions with
    act :: acts -> let init = empty_tree p act in
      let rew = default_policy p act in
      {v with rewards = v.rewards +. rew; children = backup init rew :: v.children; actions = acts; visits = v.visits + 1}
  | [] ->
      (match List.sort (child_cmp v) v.children with
        [] -> {v with visits = v.visits + 1; rewards = v.rewards +. p.reward v.state}
      | best :: rest -> let best' = tree_policy p best in
        {v with rewards = v.rewards +. best'.rewards -. best.rewards; visits = v.visits + 1; children = best' :: rest}
      )
 
let by_reward c1 c2 = compare (avg_reward c2) (avg_reward c1)

let by_visits c1 c2 = compare c2.visits c1.visits

let rec best_state cmp tree = match List.sort cmp tree.children with
    best :: _ -> best_state cmp best
  | [] -> tree.state
