let solve1 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.map (fun c -> int_of_char c - int_of_char '0') (Array.of_seq (String.to_seq line))) data) in
  let width = Array.length (mat.(0)) in
  let height = Array.length mat in
  let dp = Array.make_matrix height width [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) = 9 then
        dp.(y).(x) <- [(x, y)]
    done
  done;

  let dirs = [(1, 0); (-1, 0); (0, 1); (0, -1)] in

  for d = 8 downto 0 do
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if mat.(y).(x) = d then
          dp.(y).(x) <- 
            List.map (fun (dx, dy) ->
              let nx = x + dx in
              let ny = y + dy in
              if 0 <= nx && nx < width && 0 <= ny && ny < height &&
                mat.(ny).(nx) = d + 1
              then dp.(ny).(nx) else []) dirs
            |> List.fold_left (@) []
            |> List.sort_uniq compare;
      done
    done
  done;

  let total = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) = 0 then
        total := !total + List.length dp.(y).(x)
    done
  done;
  Printf.printf "%d\n" !total

let solve2 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.map (fun c -> int_of_char c - int_of_char '0') (Array.of_seq (String.to_seq line))) data) in
  let width = Array.length (mat.(0)) in
  let height = Array.length mat in
  let dp = Array.make_matrix height width 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) = 9 then
        dp.(y).(x) <- 1
    done
  done;

  let dirs = [(1, 0); (-1, 0); (0, 1); (0, -1)] in

  for d = 8 downto 0 do
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if mat.(y).(x) = d then
          dp.(y).(x) <- 
            List.map (fun (dx, dy) ->
              let nx = x + dx in
              let ny = y + dy in
              if 0 <= nx && nx < width && 0 <= ny && ny < height &&
                mat.(ny).(nx) = d + 1
              then dp.(ny).(nx) else 0) dirs
            |> List.fold_left (+) 0;
      done
    done
  done;

  let total = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if mat.(y).(x) = 0 then
        total := !total + dp.(y).(x)
    done
  done;
  Printf.printf "%d\n" !total
