let solve1 data =
  let mat =
    Array.of_list
      (List.map (fun line -> Array.of_seq (String.to_seq line)) data)
  in
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  let is_xmas mat x y dx dy =
    (not
       ((dx == -1 && x <= 2)
       || (dx == 1 && x >= width - 3)
       || (dy == -1 && y <= 2)
       || (dy == 1 && y >= height - 3)))
    && mat.(y).(x) = 'X'
    && mat.(y + dy).(x + dx) = 'M'
    && mat.(y + (2 * dy)).(x + (2 * dx)) = 'A'
    && mat.(y + (3 * dy)).(x + (3 * dx)) = 'S'
  in
  let count = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      List.iter
        (fun (dx, dy) -> if is_xmas mat x y dx dy then incr count)
        [ (1, 0); (1, -1); (0, -1); (-1, -1); (-1, 0); (-1, 1); (0, 1); (1, 1) ]
    done
  done;
  Printf.printf "%d\n" !count

let solve2 data =
  let mat =
    Array.of_list
      (List.map (fun line -> Array.of_seq (String.to_seq line)) data)
  in
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  let is_xmas mat x y =
    mat.(y).(x) = 'A'
    && ((mat.(y - 1).(x - 1) = 'M' && mat.(y + 1).(x + 1) = 'S')
       || (mat.(y - 1).(x - 1) = 'S' && mat.(y + 1).(x + 1) = 'M'))
    && ((mat.(y - 1).(x + 1) = 'M' && mat.(y + 1).(x - 1) = 'S')
       || (mat.(y - 1).(x + 1) = 'S' && mat.(y + 1).(x - 1) = 'M'))
  in
  let count = ref 0 in
  for y = 1 to height - 2 do
    for x = 1 to width - 2 do
      if is_xmas mat x y then incr count
    done
  done;
  Printf.printf "%d\n" !count
