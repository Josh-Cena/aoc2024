let solve1 data =
  let mat = ref (Array.of_list (List.map (fun line ->
      Array.of_list (List.of_seq (String.to_seq line))) data)) in
  let width = Array.length (!mat).(0) in
  let height = Array.length !mat in
  let start_y = match Array.find_index (fun x -> Array.exists ((=) '^') x) !mat with
    | Some y -> y
    | None -> failwith "No starting position found" in
  let start_x = match Array.find_index (fun x -> x = '^') (!mat).(start_y) with
    | Some x -> x
    | None -> failwith "No starting position found" in
  let pos = ref (start_x, start_y) in
  (!mat).(start_y).(start_x) <- 'X';
  let direction = ref (0, -1) in
  let turn_right (dx, dy) = (-dy, dx) in
  let exception Exit in
    try while true do
      let (x, y) = !pos in
      let new_x = x + fst !direction in
      let new_y = y + snd !direction in
      if new_x < 0 || new_x >= width || new_y < 0 || new_y >= height then
        raise Exit
      else if (!mat).(new_y).(new_x) = '#' then
        direction := turn_right !direction
      else
        ((!mat).(new_y).(new_x) <- 'X'; pos := (new_x, new_y))
    done;
  with Exit -> ();
  let covered = Array.fold_left (fun acc row ->
      acc + Array.fold_left (fun acc cell ->
        if cell = 'X' then acc + 1 else acc
      ) 0 row
    ) 0 !mat in
  Printf.printf "%d\n" covered

let rec windows4 = function
  | a :: b :: c :: d :: rest ->
      (a, b, c, d) :: windows4 (b :: c :: d :: rest)
  | _ -> []

let solve2 data =
  let mat = ref (Array.of_list (List.map (fun line ->
      Array.of_list (List.of_seq (String.to_seq line))) data)) in
  let width = Array.length (!mat).(0) in
  let height = Array.length !mat in
  let start_y = match Array.find_index (fun x -> Array.exists ((=) '^') x) !mat with
    | Some y -> y
    | None -> failwith "No starting position found" in
  let start_x = match Array.find_index (fun x -> x = '^') (!mat).(start_y) with
    | Some x -> x
    | None -> failwith "No starting position found" in
  let pos = ref (start_x, start_y) in
  (!mat).(start_y).(start_x) <- 'X';
  let direction = ref (0, -1) in
  let turn_right (dx, dy) = (-dy, dx) in
  let exception Exit in
    try while true do
      let (x, y) = !pos in
      let new_x = x + fst !direction in
      let new_y = y + snd !direction in
      if new_x < 0 || new_x >= width || new_y < 0 || new_y >= height then
        raise Exit
      else if (!mat).(new_y).(new_x) = '#' then
        direction := turn_right !direction
      else
        ((!mat).(new_y).(new_x) <- 'X'; pos := (new_x, new_y))
    done;
  with Exit -> ();
  let can_be_rect x y =
    let mat' = ref (Array.map Array.copy !mat) in
    (!mat').(y).(x) <- '#';
    let pos = ref (start_x, start_y) in
    let direction = ref (0, -1) in
    let path = ref (Array.map (fun row -> Array.map (fun _ -> []) row) !mat') in
    let exception Exit of bool in
      try while true do
        let (x, y) = !pos in
        let new_x = x + fst !direction in
        let new_y = y + snd !direction in
        if new_x < 0 || new_x >= width || new_y < 0 || new_y >= height then
          raise (Exit false)
        else if List.exists (fun p -> p = !direction) (!path).(new_y).(new_x) then
          raise (Exit true)
        else if (!mat').(new_y).(new_x) = '#' then
          direction := turn_right !direction
        else
          (pos := (new_x, new_y); (!path).(new_y).(new_x) <- !direction :: (!path).(new_y).(new_x))
      done;
    with Exit b -> b in
  let all_x = ref [] in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if (!mat).(y).(x) = 'X' then
        all_x := (x, y) :: !all_x
    done
  done;
  let rects = List.filter (fun (x, y) -> can_be_rect x y) !all_x in
  Printf.printf "%d\n" (List.length rects)
