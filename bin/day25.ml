let count_col (rows : string list) =
  match rows with
  | [] -> []
  | r0 :: _ ->
    let a = Array.of_list rows in
    let n = Array.length a in
    let m = String.length r0 in
    let counts = Array.make m 0 in
    for i = 0 to n - 1 do
      let s = a.(i) in
      for j = 0 to m - 1 do
        if s.[j] = '#' then counts.(j) <- counts.(j) + 1
      done
    done;
    Array.to_list counts

let solve1 data =
  let schematics = Str.split (Str.regexp "\n\n") (String.concat "\n" data) in
  let keys, locks = List.fold_left (fun (keys, locks) schematic ->
    let lines = String.split_on_char '\n' schematic in
    let cols = count_col lines in
    if String.starts_with ~prefix:"#" schematic then
      keys, (cols :: locks)
    else
      (cols :: keys), locks
  ) ([], []) schematics in
  let total = List.fold_left (fun acc key ->
    List.fold_left (fun acc lock ->
      if List.for_all2 (fun k l -> k + l <= 7) key lock then acc + 1 else acc
    ) acc locks
  ) 0 keys in
  Printf.printf "%d\n" total

let solve2 _ = Printf.printf "No such thing, yay\n"
