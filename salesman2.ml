open Batteries
open Uct2

let read_city_line line =
  try Scanf.sscanf line " %d %d %d" (fun i x y -> Some (i, x, y))
  with Scanf.Scan_failure _ -> None

let read_cities fp = File.lines_of fp |> Enum.filter_map read_city_line |> List.of_enum

let distances cities =
  let n = List.length cities in
  let mat = Array.make_matrix n n 0.0 in
  let fi = float_of_int in
  List.iter (fun (c1, x1, y1) ->
    List.iter (fun (c2, x2, y2) ->
      mat.(c1-1).(c2-1) <- sqrt (((fi (x1 - x2)) ** 2. +. (fi (y1 - y2)) ** 2.))) cities) cities;
  mat

let rec butlast l = match l with
    [] -> []
  | x :: [] -> []
  | (x :: xs) -> x :: butlast xs

let min_dist = ref 100000000.

let round_trip_dist dists path =
  List.fold_right2 (fun c1 c2 -> (+.) (dists c1 c2)) (butlast path) (List.tl path) 0.

let state_acts probf n visited =
  let score c = probf (List.hd visited) c in
  let l = List.length visited in
  if l < n then
    (1--n) |> Enum.filter (fun c -> not (List.mem c visited))
    |> Enum.map (fun c -> (c :: visited, score c)) |> List.of_enum
  else if l = n then [(1 :: visited, score 1)]
  else []

let dupl x = (x, x)

let rec ltake n l = lazy (
  if n = 0 then LazyList.Nil
  else match LazyList.get l with
      Some (x, rest) -> LazyList.Cons (x, ltake (n-1) rest)
    | None -> LazyList.Nil)

let eval distsf path =
  let d = round_trip_dist distsf path in
  min_dist := min d !min_dist;
  33000. /. d


let _ =
  let cities = read_cities "att48.tsp" in
  let dists = distances cities in
  let max_dist = Array.fold_left (Array.fold_left max) 0. dists in
  let distsf c1 c2 = dists.(c1-1).(c2-1) in
  let probf c1 c2 = 1. -. (distsf c1 c2) /. max_dist in
  let tsp_problem = {successors = state_acts probf (List.length cities); reward = eval distsf} in
  let init = empty_tree tsp_problem [1] in
  let trees = LazyList.from_loop init (tree_policy tsp_problem %> dupl) in
  trees
  |> ltake 15000
  |> LazyList.iter (fun _ -> Format.printf "%f\n" !min_dist)
