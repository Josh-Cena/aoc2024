module IntMap = Map.Make (Int)

let counts lst =
  List.fold_left
    (fun acc x ->
      IntMap.update x (function None -> Some 1 | Some n -> Some (n + 1)) acc)
    IntMap.empty lst

let evolve nums =
  IntMap.fold
    (fun x count acc ->
      let sx = string_of_int x in
      let new_x =
        if x = 0 then [ 1 ]
        else if String.length sx mod 2 = 0 then
          [
            int_of_string (String.sub sx 0 (String.length sx / 2));
            int_of_string
              (String.sub sx (String.length sx / 2) (String.length sx / 2));
          ]
        else [ x * 2024 ]
      in
      List.fold_left
        (fun acc' nx ->
          IntMap.update nx
            (function None -> Some count | Some c -> Some (c + count))
            acc')
        acc new_x)
    nums IntMap.empty

let rec evolve_n n nums = if n = 0 then nums else evolve_n (n - 1) (evolve nums)

let solve n data =
  let line = List.hd data in
  let nums = counts (List.map int_of_string (String.split_on_char ' ' line)) in
  let final_nums = evolve_n n nums in
  Printf.printf "%d\n"
    (IntMap.fold (fun _ count acc -> acc + count) final_nums 0)

let solve1 = solve 25
let solve2 = solve 75
