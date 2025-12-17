let rec ways_to_construct pattern towels memo =
  match Hashtbl.find_opt memo pattern with
  | Some x -> x
  | None ->
    let ways = List.map (fun prefix ->
      if String.starts_with ~prefix pattern then
        ways_to_construct (String.sub pattern (String.length prefix) (String.length pattern - String.length prefix)) towels memo
      else
        0 ) towels in
    let result = List.fold_left (+) 0 ways in
    Hashtbl.add memo pattern result;
    result

let solve1 data =
  let towels = Str.split (Str.regexp ", ") (List.hd data) in
  let patterns = List.tl (List.tl data) in
  let memo = Hashtbl.create (List.length patterns * List.length patterns) in
  Hashtbl.add memo "" 1;
  let ways = List.map (fun pattern -> ways_to_construct pattern towels memo) patterns in
  Printf.printf "%d\n" (List.length (List.filter (fun x -> x > 0) ways))

let solve2 data =
  let towels = Str.split (Str.regexp ", ") (List.hd data) in
  let patterns = List.tl (List.tl data) in
  let memo = Hashtbl.create (List.length patterns * List.length patterns) in
  Hashtbl.add memo "" 1;
  let ways = List.map (fun pattern -> ways_to_construct pattern towels memo) patterns in
  Printf.printf "%d\n" (List.fold_left (+) 0 ways)
