let char_to_dir c =
  match c with
  | '^' -> (-1, 0)
  | '>' -> (0, 1)
  | 'v' -> (1, 0)
  | '<' -> (0, -1)
  | _ -> failwith "Invalid direction character"

let solve1 data =
  let parts = Str.split (Str.regexp "\n\n") (String.concat "\n" data) in
  let mat =
    List.hd parts |> String.split_on_char '\n'
    |> List.map (fun line -> line |> String.to_seq |> Array.of_seq)
    |> Array.of_list |> ref
  in
  let w = Array.length !mat.(0) in
  let h = Array.length !mat in
  let rec can_move r c dr dc =
    let nr = r + dr in
    let nc = c + dc in
    match !mat.(nr).(nc) with
    | '#' -> false
    | '.' -> true
    | 'O' -> can_move nr nc dr dc
    | _ -> failwith "Invalid cell"
  in
  let rec do_move r c dr dc =
    let nr = r + dr in
    let nc = c + dc in
    if !mat.(nr).(nc) = 'O' then do_move nr nc dr dc;
    !mat.(nr).(nc) <- !mat.(r).(c);
    !mat.(r).(c) <- '.'
  in
  let start_r =
    Option.get (Array.find_index (fun x -> Array.exists (( = ) '@') x) !mat)
  in
  let start_c = Option.get (Array.find_index (( = ) '@') !mat.(start_r)) in
  let pos = ref (start_r, start_c) in
  let dirs =
    List.map char_to_dir (List.of_seq (String.to_seq (List.nth parts 1)))
  in
  List.iter
    (fun (dr, dc) ->
      let r, c = !pos in
      if can_move r c dr dc then begin
        do_move r c dr dc;
        pos := (r + dr, c + dc)
      end)
    dirs;
  let total = ref 0 in
  for r = 1 to h - 2 do
    for c = 1 to w - 2 do
      if !mat.(r).(c) = 'O' then total := !total + (r * 100) + c
    done
  done;
  Printf.printf "%d\n" !total

let solve2 data =
  let parts = Str.split (Str.regexp "\n\n") (String.concat "\n" data) in
  let mat =
    List.hd parts |> String.split_on_char '\n'
    |> List.map (fun line ->
        line |> String.to_seq
        |> Seq.flat_map (fun c ->
            match c with
            | '#' -> List.to_seq [ '#'; '#' ]
            | 'O' -> List.to_seq [ '['; ']' ]
            | '.' -> List.to_seq [ '.'; '.' ]
            | '@' -> List.to_seq [ '@'; '.' ]
            | _ -> failwith "Invalid character")
        |> Array.of_seq)
    |> Array.of_list |> ref
  in
  let w = Array.length !mat.(0) in
  let h = Array.length !mat in
  let rec can_move r c dr dc =
    let nr = r + dr in
    let nc = c + dc in
    match !mat.(nr).(nc) with
    | '#' -> false
    | '.' -> true
    | ']' -> begin
        match (dr, dc) with
        | 0, 1 -> failwith "Shouldn't be looking at ] when moving right"
        | 0, -1 -> can_move nr (nc - 1) dr dc
        | _ -> can_move nr nc dr dc && can_move nr (nc - 1) dr dc
      end
    | '[' -> begin
        match (dr, dc) with
        | 0, 1 -> can_move nr (nc + 1) dr dc
        | 0, -1 -> failwith "Shouldn't be looking at [ when moving left"
        | _ -> can_move nr nc dr dc && can_move nr (nc + 1) dr dc
      end
    | _ -> failwith "Invalid cell"
  in
  let rec do_move r c dr dc =
    let nr = r + dr in
    let nc = c + dc in
    if !mat.(nr).(nc) = ']' then begin
      match (dr, dc) with
      | 0, 1 | 0, -1 -> do_move nr nc dr dc
      | _ ->
          do_move nr (nc - 1) dr dc;
          do_move nr nc dr dc
    end
    else if !mat.(nr).(nc) = '[' then begin
      match (dr, dc) with
      | 0, 1 | 0, -1 -> do_move nr nc dr dc
      | _ ->
          do_move nr nc dr dc;
          do_move nr (nc + 1) dr dc
    end;
    !mat.(nr).(nc) <- !mat.(r).(c);
    !mat.(r).(c) <- '.'
  in
  let start_r =
    Option.get (Array.find_index (fun x -> Array.exists (( = ) '@') x) !mat)
  in
  let start_c = Option.get (Array.find_index (( = ) '@') !mat.(start_r)) in
  let pos = ref (start_r, start_c) in
  let dirs =
    List.map char_to_dir (List.of_seq (String.to_seq (List.nth parts 1)))
  in
  List.iter
    (fun (dr, dc) ->
      let r, c = !pos in
      if can_move r c dr dc then begin
        do_move r c dr dc;
        pos := (r + dr, c + dc)
      end)
    dirs;
  let total = ref 0 in
  for r = 1 to h - 2 do
    for c = 1 to w - 2 do
      if !mat.(r).(c) = '[' then total := !total + (r * 100) + c
    done
  done;
  Printf.printf "%d\n" !total
