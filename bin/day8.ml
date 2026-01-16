let rec combinations2 = function
  | [] | [ _ ] -> []
  | x :: xs ->
      let with_x = List.map (fun y -> (x, y)) xs in
      with_x @ combinations2 xs

let parse_input data =
  let mat =
    Array.of_list
      (List.map (fun line -> Array.of_seq (String.to_seq line)) data)
  in
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  let antenna_groups = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) <> '.' then
        if List.mem_assoc mat.(y).(x) !antenna_groups then (
          let group = List.assoc mat.(y).(x) !antenna_groups in
          antenna_groups := List.remove_assoc mat.(y).(x) !antenna_groups;
          antenna_groups := (mat.(y).(x), (x, y) :: group) :: !antenna_groups)
        else antenna_groups := (mat.(y).(x), [ (x, y) ]) :: !antenna_groups
    done
  done;
  (!antenna_groups, mat, width, height)

let solve1 data =
  let antenna_groups, mat, width, height = parse_input data in
  let antinodes = Array.map (Array.map (fun _ -> false)) mat in
  List.iter
    (fun (_, positions) ->
      List.iter
        (fun ((x1, y1), (x2, y2)) ->
          let dx = x2 - x1 in
          let dy = y2 - y1 in
          if 0 <= y2 + dy && y2 + dy < height && 0 <= x2 + dx && x2 + dx < width
          then antinodes.(y2 + dy).(x2 + dx) <- true;
          if 0 <= y1 - dy && y1 - dy < height && 0 <= x1 - dx && x1 - dx < width
          then antinodes.(y1 - dy).(x1 - dx) <- true)
        (combinations2 positions))
    antenna_groups;
  let count = ref 0 in
  Array.iter
    (fun row -> Array.iter (fun cell -> if cell then incr count) row)
    antinodes;
  Printf.printf "%d\n" !count

let solve2 data =
  let antenna_groups, mat, width, height = parse_input data in
  let antinodes = Array.map (Array.map (fun _ -> false)) mat in
  List.iter
    (fun (_, positions) ->
      List.iter
        (fun ((x1, y1), (x2, y2)) ->
          let dx = x2 - x1 in
          let dy = y2 - y1 in
          let rec loop m =
            let nx1 = x1 + (m * dx) in
            let ny1 = y1 + (m * dy) in
            let nx2 = x1 - (m * dx) in
            let ny2 = y1 - (m * dy) in
            let in_bounds1 =
              0 <= nx1 && nx1 < width && 0 <= ny1 && ny1 < height
            in
            let in_bounds2 =
              0 <= nx2 && nx2 < width && 0 <= ny2 && ny2 < height
            in
            if in_bounds1 then antinodes.(ny1).(nx1) <- true;
            if in_bounds2 then antinodes.(ny2).(nx2) <- true;
            if (not in_bounds1) && not in_bounds2 then () else loop (m + 1)
          in
          loop 0)
        (combinations2 positions))
    antenna_groups;
  let count = ref 0 in
  Array.iter
    (fun row -> Array.iter (fun cell -> if cell then incr count) row)
    antinodes;
  Printf.printf "%d\n" !count
