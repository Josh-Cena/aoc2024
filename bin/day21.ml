(*
0 |  <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
1 |    v <<   A >>  ^ A   <   A > A  v  A   <  ^ AA > A   < v  AAA >  ^ A
2 |           <       A       ^   A     >        ^^   A        vvv      A
3 |                   0           2                   9                 A

For each layer, the optimal path from pressing one button to pressing the next
only depends on the two buttons and not any state from higher layers. So we can
first design a path on the lower layer and then ask the higher layer to
implement it.

There are some obvious constraints for the optimal path:
- At no point should it move *away* from the target.
- If it's going in one direction, it should go all the way.
- If it needs to move both horizontally and vertically, first consider if one
  of the two choices is blocked by the gap; if so, use the other one; otherwise,
  we need to consider what happens on the higher layers. Consider ^<:

  2 |  A       ^        <       A
  1 |  A   <   A   < v  A >>  ^ A
  0 |  Av<<A>>^Av<<A>A^>AvAA<^A>A

  2 |  A         <      ^   A
  1 |  A  v <<   A ^  > A > A   => LU
  0 |  A<vA<AA>>^A<Av>A>AvA^A

  On layer 0, certain choices are equivalent. From this comparison, <^ is
  better. Why? Because on layer 1, it does the expensive < moves together.

  Similarly, consider the other three combinations:

  2 |  A   ^      >   A
  1 |  A < A  v > A ^ A         => UR
  0 |  A<A>A<vA>A^A<A>A

  2 |  A     >        ^   A
  1 |  A  v  A   <  ^ A > A
  0 |  A<vA^>Av<<A^>A>AvA^A

  2 |  A        v       <       A
  1 |  A   < v  A   <   A >>  ^ A
  0 |  Av<<A>A^>Av<<A>>^AvAA<^A>A

  2 |  A         <   v      A
  1 |  A  v <<   A > A ^  > A   => LD
  0 |  A<vA<AA>>^AvA^A<Av>A^A

  2 |  A        v   >   A
  1 |  A   < v  A > A ^ A        => DR
  0 |  Av<<A>A^>AvA^A<A>A

  2 |  A     >       v      A
  1 |  A  v  A   <   A ^  > A
  0 |  A<vA^>Av<<A>>^A<Av>A^A

  So the conclusion is: we should choose L > D > U > R.
*)

(* (x, y) coordinates with A as origin *)
(* The gap is at (-2, 0) for both pads *)
let numpad =
  Hashtbl.of_seq
    (List.to_seq
       [
         ('7', (-2, 3));
         ('8', (-1, 3));
         ('9', (0, 3));
         ('4', (-2, 2));
         ('5', (-1, 2));
         ('6', (0, 2));
         ('1', (-2, 1));
         ('2', (-1, 1));
         ('3', (0, 1));
         ('0', (-1, 0));
         ('A', (0, 0));
       ])

let dirpad =
  Hashtbl.of_seq
    (List.to_seq
       [
         ('^', (-1, 0));
         ('A', (0, 0));
         ('<', (-2, -1));
         ('v', (-1, -1));
         ('>', (0, -1));
       ])

(* Only handles one direction *)
let move_seq dx dy =
  match (compare dx 0, compare dy 0) with
  | 0, 0 -> []
  | 0, 1 -> List.init dy (fun _ -> '^')
  | 0, -1 -> List.init (-dy) (fun _ -> 'v')
  | 1, 0 -> List.init dx (fun _ -> '>')
  | -1, 0 -> List.init (-dx) (fun _ -> '<')
  | _ -> failwith "Invalid move"

(* If currently at from, how should it press the button at to (including
   trailing A) *)
let optimal_path (from_x, from_y) (to_x, to_y) =
  let dx, dy = (to_x - from_x, to_y - from_y) in
  if dx = 0 || dy = 0 then move_seq dx dy @ [ 'A' ]
  else
    let interm1, interm2 = ((from_x + dx, from_y), (from_x, from_y + dy)) in
    if interm1 = (-2, 0) then move_seq 0 dy @ move_seq dx 0 @ [ 'A' ]
    else if interm2 = (-2, 0) then move_seq dx 0 @ move_seq 0 dy @ [ 'A' ]
    else if dx < 0 then
      (* LD, LU *)
      move_seq dx 0 @ move_seq 0 dy @ [ 'A' ]
    else
      (* DR, UR *)
      move_seq 0 dy @ move_seq dx 0 @ [ 'A' ]

(* If currently at from on layer depth, how many clicks by *you* to reach
   to (including trailing A) *)
let rec optimal_path_cost from_pos to_pos depth memo =
  if Hashtbl.mem memo (from_pos, to_pos, depth) then
    Hashtbl.find memo (from_pos, to_pos, depth)
  else
    let path = optimal_path from_pos to_pos in
    let total_cost =
      if depth = 1 then List.length path
      else
        optimal_total_path_cost
          (List.map (Hashtbl.find dirpad) ('A' :: path))
          (depth - 1) memo
    in
    Hashtbl.add memo (from_pos, to_pos, depth) total_cost;
    total_cost

and optimal_total_path_cost poses depth memo =
  let rec aux acc = function
    | [] | [ _ ] -> acc
    | step1 :: step2 :: rest ->
        let step_cost = optimal_path_cost step1 step2 depth memo in
        aux (acc + step_cost) (step2 :: rest)
  in
  aux 0 poses

let solve depth data =
  let memo = Hashtbl.create 100 in
  let total =
    List.fold_left
      (fun acc line ->
        let chars = List.of_seq (String.to_seq line) in
        let positions =
          List.map (fun c -> Hashtbl.find numpad c) ('A' :: chars)
        in
        let cost = optimal_total_path_cost positions depth memo in
        let numeric =
          int_of_string (String.sub line 0 (String.length line - 1))
        in
        acc + (cost * numeric))
      0 data
  in
  Printf.printf "%d\n" total

let solve1 = solve 3
let solve2 = solve 26
