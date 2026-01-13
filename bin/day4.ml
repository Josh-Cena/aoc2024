let solve1 data =
  let mat =
    Array.of_list
      (List.map (fun line -> Array.of_seq (String.to_seq line)) data)
  in
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  let is_xmas mat x y dirx diry =
    (not
       ((dirx == -1 && x <= 2)
       || (dirx == 1 && x >= width - 3)
       || (diry == -1 && y <= 2)
       || (diry == 1 && y >= height - 3)))
    && mat.(y).(x) = 'X'
    && mat.(y + diry).(x + dirx) = 'M'
    && mat.(y + (2 * diry)).(x + (2 * dirx)) = 'A'
    && mat.(y + (3 * diry)).(x + (3 * dirx)) = 'S'
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
    x >= 1
    && x <= width - 2
    && y >= 1
    && y <= height - 2
    && mat.(y).(x) = 'A'
    && ((mat.(y - 1).(x - 1) = 'M' && mat.(y + 1).(x + 1) = 'S')
       || (mat.(y - 1).(x - 1) = 'S' && mat.(y + 1).(x + 1) = 'M'))
    && ((mat.(y - 1).(x + 1) = 'M' && mat.(y + 1).(x - 1) = 'S')
       || (mat.(y - 1).(x + 1) = 'S' && mat.(y + 1).(x - 1) = 'M'))
  in
  let count = ref 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      if is_xmas mat x y then incr count
    done
  done;
  Printf.printf "%d\n" !count
