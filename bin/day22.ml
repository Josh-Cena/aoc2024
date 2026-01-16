let next_secret num =
  let num' = (num lsl 6) lxor num land 0xFFFFFF in
  let num'' = (num' lsr 5) lxor num' in
  let num''' = (num'' lsl 11) lxor num'' land 0xFFFFFF in
  num'''

let rec rep f n acc = if n = 0 then acc else rep f (n - 1) (f acc)

(* Computes the gains for each 4-window existing in a single initial secret *)
let gains init_num =
  let map = Array.make 130321 0 in
  let n1 = init_num mod 10 in
  let num2 = next_secret init_num in
  let n2 = num2 mod 10 in
  let d1 = n2 - n1 in
  let num3 = next_secret num2 in
  let n3 = num3 mod 10 in
  let d2 = n3 - n2 in
  let num4 = next_secret num3 in
  let n4 = num4 mod 10 in
  let d3 = n4 - n3 in
  let num5 = next_secret num4 in
  let n5 = num5 mod 10 in
  let d4 = n5 - n4 in
  (* Pack the 4-window into a base 19 number *)
  let key = ((d1 + 9) * 6859) + ((d2 + 9) * 361) + ((d3 + 9) * 19) + (d4 + 9) in
  map.(key) <- n5;
  let rec loop num n key times =
    if times = 0 then map
    else
      let num' = next_secret num in
      let n' = num' mod 10 in
      let key' = ((key * 19) + (n' - n + 9)) mod 130321 in
      if map.(key') = 0 then map.(key') <- n';
      loop num' n' key' (times - 1)
  in
  loop num5 n5 key 1996

let sum_arrays a b = Array.init (Array.length a) (fun i -> a.(i) + b.(i))

let solve1 data =
  let nums = List.map int_of_string data in
  let nums' = List.map (rep next_secret 2000) nums in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 nums')

let solve2 data =
  let nums = List.map int_of_string data in
  let gains_sum =
    List.fold_left
      (fun acc num -> sum_arrays acc (gains num))
      (Array.make 130321 0) nums
  in
  let max_gain = Array.fold_left max 0 gains_sum in
  Printf.printf "%d\n" max_gain
