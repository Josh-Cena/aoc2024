let num_working_cheats distances i j radius =
  let count = ref 0 in
  let h = Array.length distances in
  let w = Array.length distances.(0) in
  for ni = i - radius to i + radius do
    for nj = j - radius + abs (i - ni) to j + radius - abs (i - ni) do
      if ni < 0 || ni >= h || nj < 0 || nj >= w then ()
      else if distances.(ni).(nj) - distances.(i).(j) >= 100 + abs(i - ni) + abs(j - nj) then
        count := !count + 1
    done
  done;
  !count

let parse_input data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_seq (String.to_seq line)) data) in
  let distance = ref 0 in
  let start_pos = Option.get (Array.find_mapi (fun i row ->
    Array.find_mapi (fun j c -> if c = 'S' then Some (i, j) else None) row) mat) in
  let cur = ref start_pos in
  let distances = Array.make_matrix (Array.length mat) (Array.length mat.(0)) (-1) in
  let path = ref [] in
  let exception Exit in
    try while true do
      let (i, j) = !cur in
      distances.(i).(j) <- !distance;
      path := !cur :: !path;
      distance := !distance + 1;
      if mat.(i).(j) = 'E' then
        raise Exit
      else begin
        let new_pos = List.find (fun (ni, nj) ->
          if ni < 0 || ni >= Array.length mat || nj < 0 || nj >= Array.length mat.(0) then false
          else if distances.(ni).(nj) <> -1 then false
          else mat.(ni).(nj) <> '#') [(i+1, j); (i-1, j); (i, j+1); (i, j-1)] in
        cur := new_pos
      end
    done
  with Exit -> ();
  (distances, List.rev !path)

let solve1 data =
  let (distances, path) = parse_input data in
  let cheats = List.map (fun (i, j) -> num_working_cheats distances i j 2) path in
  Printf.printf "%d\n" (List.fold_left (+) 0 cheats)

let solve2 data =
  let (distances, path) = parse_input data in
  let cheats = List.map (fun (i, j) -> num_working_cheats distances i j 20) path in
  Printf.printf "%d\n" (List.fold_left (+) 0 cheats)
