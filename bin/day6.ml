let turn_right (dx, dy) = (-dy, dx)

let create_walked_mat data =
  let mat =
    ref
      (Array.of_list
         (List.map (fun line -> Array.of_seq (String.to_seq line)) data))
  in
  let width = Array.length !mat.(0) in
  let height = Array.length !mat in
  let start_y, start_x =
    !mat
    |> Array.find_mapi (fun i ->
        Array.find_mapi (fun j c -> if c = '^' then Some (i, j) else None))
    |> Option.get
  in
  !mat.(start_y).(start_x) <- 'X';
  let rec loop pos dir =
    let x, y = pos in
    let dx, dy = dir in
    let new_x = x + dx in
    let new_y = y + dy in

    if new_x < 0 || new_x >= width || new_y < 0 || new_y >= height then ()
    else if !mat.(new_y).(new_x) = '#' then loop pos (turn_right dir)
    else (
      !mat.(new_y).(new_x) <- 'X';
      loop (new_x, new_y) dir)
  in
  loop (start_x, start_y) (0, -1);
  (!mat, start_x, start_y, width, height)

let solve1 data =
  let mat, _, _, _, _ = create_walked_mat data in
  let covered = ref 0 in
  Array.iter
    (fun row -> Array.iter (fun cell -> if cell = 'X' then incr covered) row)
    mat;
  Printf.printf "%d\n" !covered

let rec windows4 = function
  | a :: b :: c :: d :: rest -> (a, b, c, d) :: windows4 (b :: c :: d :: rest)
  | _ -> []

let solve2 data =
  let mat, start_x, start_y, width, height = create_walked_mat data in
  let can_be_rect x y =
    let mat' = Array.map Array.copy mat in
    mat'.(y).(x) <- '#';
    let path = ref (Array.map (fun row -> Array.map (fun _ -> []) row) mat') in
    let rec loop pos dir =
      let x, y = pos in
      let dx, dy = dir in
      let new_x = x + dx in
      let new_y = y + dy in

      if new_x < 0 || new_x >= width || new_y < 0 || new_y >= height then false
      else if List.exists (fun p -> p = dir) !path.(new_y).(new_x) then true
      else if mat'.(new_y).(new_x) = '#' then loop pos (turn_right dir)
      else (
        !path.(new_y).(new_x) <- dir :: !path.(new_y).(new_x);
        loop (new_x, new_y) dir)
    in
    loop (start_x, start_y) (0, -1)
  in
  let num_rects = ref 0 in
  Array.iteri
    (fun y row ->
      Array.iteri
        (fun x cell -> if cell = 'X' && can_be_rect x y then incr num_rects)
        row)
    mat;
  Printf.printf "%d\n" !num_rects
