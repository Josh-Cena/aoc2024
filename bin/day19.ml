let rec ways_to_construct pattern towels memo =
  match Hashtbl.find_opt memo pattern with
  | Some count -> count
  | None ->
      let count =
        towels
        |> List.filter (fun towel -> String.starts_with ~prefix:towel pattern)
        |> List.map (fun towel ->
            ways_to_construct
              (String.sub pattern (String.length towel)
                 (String.length pattern - String.length towel))
              towels memo)
        |> List.fold_left ( + ) 0
      in
      Hashtbl.add memo pattern count;
      count

let solve1 data =
  let towels = Str.split (Str.regexp ", ") (List.hd data) in
  let patterns = List.tl (List.tl data) in
  let memo = Hashtbl.create (List.length patterns * List.length patterns) in
  Hashtbl.add memo "" 1;
  let ways =
    List.map (fun pattern -> ways_to_construct pattern towels memo) patterns
  in
  Printf.printf "%d\n" (List.length (List.filter (fun x -> x > 0) ways))

let solve2 data =
  let towels = Str.split (Str.regexp ", ") (List.hd data) in
  let patterns = List.tl (List.tl data) in
  let memo = Hashtbl.create (List.length patterns * List.length patterns) in
  Hashtbl.add memo "" 1;
  let ways =
    List.map (fun pattern -> ways_to_construct pattern towels memo) patterns
  in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 ways)
