let rec combinations2 = function
  | [] | [_] -> []
  | x :: xs ->
      let with_x = List.map (fun y -> (x, y)) xs in
      with_x @ combinations2 xs

let solve1 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_list (List.of_seq (String.to_seq line))) data) in
  let width = Array.length (mat.(0)) in
  let height = Array.length mat in
  let antenna_groups = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) <> '.' then
        if List.mem_assoc (mat.(y).(x)) !antenna_groups then
          let group = List.assoc (mat.(y).(x)) !antenna_groups in
          antenna_groups := List.remove_assoc (mat.(y).(x)) !antenna_groups;
          antenna_groups := (mat.(y).(x), (x, y) :: group) :: !antenna_groups
        else
          antenna_groups := (mat.(y).(x), [(x, y)]) :: !antenna_groups
    done
  done;
  let antinodes = Array.map (Array.map (fun _ -> false)) mat in
  List.iter (fun (_, positions) ->
    List.iter (fun ((x1, y1), (x2, y2)) ->
      let dx = x2 - x1 in
      let dy = y2 - y1 in
      if 0 <= y2 + dy && y2 + dy < height && 0 <= x2 + dx && x2 + dx < width then
        antinodes.(y2 + dy).(x2 + dx) <- true;
      if 0 <= y1 - dy && y1 - dy < height && 0 <= x1 - dx && x1 - dx < width then
        antinodes.(y1 - dy).(x1 - dx) <- true
    ) (combinations2 positions)
  ) !antenna_groups;
  let count = Array.fold_left (fun acc row ->
      acc + (Array.fold_left (fun acc2 cell -> if cell then acc2 + 1 else acc2) 0 row)
  ) 0 antinodes in
  Printf.printf "%d\n" count

let solve2 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_list (List.of_seq (String.to_seq line))) data) in
  let width = Array.length (mat.(0)) in
  let height = Array.length mat in
  let antenna_groups = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) <> '.' then
        if List.mem_assoc (mat.(y).(x)) !antenna_groups then
          let group = List.assoc (mat.(y).(x)) !antenna_groups in
          antenna_groups := List.remove_assoc (mat.(y).(x)) !antenna_groups;
          antenna_groups := (mat.(y).(x), (x, y) :: group) :: !antenna_groups
        else
          antenna_groups := (mat.(y).(x), [(x, y)]) :: !antenna_groups
    done
  done;
  let antinodes = Array.map (Array.map (fun _ -> false)) mat in
  List.iter (fun (_, positions) ->
    List.iter (fun ((x1, y1), (x2, y2)) ->
      let dx = x2 - x1 in
      let dy = y2 - y1 in
      let m = ref 0 in
      let exception Exit in
      try while true do
        let p1 = (x2 + dx * !m, y2 + dy * !m) in
        let p2 = (x2 - dx * !m, y2 - dy * !m) in
        let in_bounds1 = let (x, y) = p1 in 0 <= x && x < width && 0 <= y && y < height in
        let in_bounds2 = let (x, y) = p2 in 0 <= x && x < width && 0 <= y && y < height in
        if in_bounds1 then antinodes.(snd p1).(fst p1) <- true;
        if in_bounds2 then antinodes.(snd p2).(fst p2) <- true;
        if not in_bounds1 && not in_bounds2 then raise Exit;
        m := !m + 1;
      done; with Exit -> ();
    ) (combinations2 positions)
  ) !antenna_groups;
  let count = Array.fold_left (fun acc row ->
      acc + (Array.fold_left (fun acc2 cell -> if cell then acc2 + 1 else acc2) 0 row)
  ) 0 antinodes in
  Printf.printf "%d\n" count
