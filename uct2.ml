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
; embryos : ('s * probability) list
; children : 's tree list
}

let empty_tree p s = {
  state = s
; visits = 0
; rewards = 0.
; embryos = p.successors s
; children = []
}

let avg_reward tree = tree.rewards /. float_of_int tree.visits

let child_score parent child =
  let c_p = sqrt 2. and fi = float_of_int in
  avg_reward child +. c_p *. sqrt (log (fi parent.visits) /. fi child.visits)

let child_cmp parent c1 c2 =
  compare (child_score parent c2) (child_score parent c1)

(* cdf [("a", 0.1); ("b", 0.9)] =
     ([("b", (0.1, 0.9, 1.)); ("a", (0., 0.1, 0.1))], 1.)
*)
let cdf l = List.fold_left (fun (acc, sum) (x, w) ->
  let sum' = sum +. w in ((x, (sum, w, sum')) :: acc, sum')) ([], 0.0) l

let cdf_sample xs =
  let (xs', lim) = cdf xs in
  let r = Random.float lim in
  xs', fun (x, (min, _, max)) -> min <= r && r <= max

let weighted_draw xs =
  let xs', crit = cdf_sample xs in
  let (inr, ofr) = List.partition crit xs' in
  List.hd inr, List.map (fun (x, (_, w, _)) -> x, w) (List.tl inr @ ofr)

let weighted_sample xs =
  let xs', crit = cdf_sample xs in List.find crit xs'

let uniform_sample xs =
  List.nth xs (Random.int (List.length xs))

let greedy_sample xs = List.hd (List.sort (fun (_, x) (_, y) -> compare y x) xs)

let rec default_policy p s =
  match p.successors s with
    [] -> p.reward s
  | xs -> default_policy p (fst (weighted_sample xs))

let visit v rew = {v with visits = v.visits + 1; rewards = v.rewards +. rew}

let rec tree_policy p v =
  if v.embryos == [] then
    match List.sort (child_cmp v) v.children with
      [] -> visit v (p.reward v.state)
    | best :: rest ->
        let best' = tree_policy p best in
        let v' = visit v (best'.rewards -. best.rewards) in
        {v' with children = best' :: rest}
  else
    let (chosen, _), rest = weighted_draw v.embryos in
    let baby = empty_tree p chosen in
    let rew = default_policy p chosen in
    let v' = visit v rew in
    {v' with children = visit baby rew :: v.children; embryos = rest}
 
let by_reward c1 c2 = compare (avg_reward c2) (avg_reward c1)

let by_visits c1 c2 = compare c2.visits c1.visits

let rec best_state cmp tree = match List.sort cmp tree.children with
    best :: _ -> best_state cmp best
  | [] -> tree.state
