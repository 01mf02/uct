type reward_t = float
type 's problem = 's -> ('s * reward_t) list

type 's tree = {
  state : 's
; visits : int
; rewards : reward_t
; children : ('s tree * reward_t) list
; actions : ('s * reward_t) list
}

let empty_tree problem s = {
  state = s
; visits = 0
; rewards = 0.
; children = []
; actions = List.sort (fun (_, x) (_, y) -> compare y x) (problem s)
}

let avg_reward tree = tree.rewards /. float_of_int tree.visits

let child_score parent child =
  let c_p = sqrt 2. and fi = float_of_int in
  avg_reward child +. c_p *. sqrt (log (fi parent.visits) /. fi child.visits)

let child_cmp parent (c1, _) (c2, _) =
  compare (child_score parent c2) (child_score parent c1)

let rec search p tree = match tree.actions with
    (act, rew) :: acts -> update p (empty_tree p act, rew) {tree with actions = acts}
  | [] ->
      (match List.sort (child_cmp tree) tree.children with
        [] -> {tree with visits = tree.visits + 1}
      | best :: rest -> update p best {tree with children = rest})
and update p (child, rew) parent =
  let gamma = 0.7
  and child' = search p child in
  { parent with
    rewards = parent.rewards +. rew +. gamma *. (child'.rewards -. child.rewards)
  ; visits = parent.visits + 1
  ; children = (child', rew) :: parent.children
  }

let by_reward (c1, _) (c2, _) = compare (avg_reward c2) (avg_reward c1)

let by_visits (c1, _) (c2, _) = compare c2.visits c1.visits

let rec best_state cmp tree = match List.sort cmp tree.children with
    (best, _) :: _ -> best_state cmp best
  | [] -> tree.state

