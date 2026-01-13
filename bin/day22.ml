let next_secret num =
  let num' = (num lsl 6) lxor num land 0xFFFFFF in
  let num'' = (num' lsr 5) lxor num' in
  let num''' = (num'' lsl 11) lxor num'' land 0xFFFFFF in
  num'''

let ( >> ) f g x = g (f x)
let rec rep f n acc = if n = 0 then acc else rep f (n - 1) (f acc)

let rec rep_history f n acc =
  if n = 0 then [ acc ] else acc :: rep_history f (n - 1) (f acc)

let running_diff lst =
  let rec helper lst acc =
    match lst with
    | [] | [ _ ] -> acc
    | x :: y :: xs -> helper (y :: xs) ((y - x, y) :: acc)
  in
  List.rev (helper lst [])

let rec windows4 lst =
  match lst with
  | [] | [ _ ] | [ _; _ ] | [ _; _; _ ] -> []
  | x :: y :: z :: w :: xs -> (x, y, z, w) :: windows4 (y :: z :: w :: xs)

let solve1 data =
  let nums = List.map int_of_string data in
  let nums' = List.map (rep next_secret 2000) nums in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 nums')

let solve2 data =
  let nums = List.map int_of_string data in
  let nums' = List.map (rep_history next_secret 2000) nums in
  let prices = List.map (List.map (fun x -> x mod 10)) nums' in
  let diffs = List.map running_diff prices in
  let total_gain =
    Hashtbl.create (List.length diffs * List.length (List.hd diffs))
  in
  List.iter
    (fun diff_seq ->
      let seq_gain = Hashtbl.create (List.length diff_seq) in
      List.iter
        (fun ((d1, _), (d2, _), (d3, _), (d4, p4)) ->
          let key = (d1, d2, d3, d4) in
          if not (Hashtbl.mem seq_gain key) then Hashtbl.add seq_gain key p4)
        (windows4 diff_seq);
      Hashtbl.iter
        (fun key gain ->
          let sum =
            Option.fold ~none:gain ~some:(( + ) gain)
              (Hashtbl.find_opt total_gain key)
          in
          Hashtbl.replace total_gain key sum)
        seq_gain)
    diffs;
  let max_gain = Hashtbl.fold (fun _ gain acc -> max acc gain) total_gain 0 in
  Printf.printf "%d\n" max_gain
