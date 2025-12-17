let solve1 data =
  let w = 71 in
  let h = 71 in
  let points = List.map (fun s ->
    let x, y = Scanf.sscanf s "%d,%d" (fun x y -> x, y) in
    (x, y)
  ) data in
  let points = List.take 1024 points in
  let grid = Array.make_matrix w h false in
  List.iter (fun (x, y) -> grid.(y).(x) <- true) points;
  let queue = Queue.create () in
  Queue.push (0, 0) queue;
  let dist = Array.make_matrix w h (-1) in
  dist.(0).(0) <- 0;
  while not (Queue.is_empty queue) do
    let x, y = Queue.pop queue in
    List.iter (fun (dx, dy) ->
      let nx, ny = x + dx, y + dy in
      if nx >= 0 && nx < w && ny >= 0 && ny < h && not grid.(ny).(nx) && dist.(ny).(nx) = -1 then begin
        dist.(ny).(nx) <- dist.(y).(x) + 1;
        Queue.push (nx, ny) queue
      end
    ) [(0, 1); (1, 0); (0, -1); (-1, 0)];
  done;
  Printf.printf "%d\n" dist.(h - 1).(w - 1)

(* The observation is this: if a wall completely separates the top left from
   the bottom right, then it must touch one of the top/right walls, and one
   of the bottom/left walls. So we can use a union-find to keep track of when
   the top/right walls get merged with the bottom/left.
   The walls have coordinate 0, so all other points are shifted by 1. *)
let solve2 data =
  (* Actual board width + walls *)
  let w = 73 in
  let h = 73 in
  let points = List.map (fun s ->
    let x, y = Scanf.sscanf s "%d,%d" (fun x y -> x + 1, y + 1) in
    (x, y)
  ) data in
  let cur_grid = Array.make_matrix w h false in
  for x = 1 to w - 2 do
    cur_grid.(0).(x) <- true;
    cur_grid.(h - 1).(x) <- true
  done;
  cur_grid.(0).(w - 1) <- true;
  for y = 1 to h - 2 do
    cur_grid.(y).(0) <- true;
    cur_grid.(y).(w - 1) <- true
  done;
  cur_grid.(h - 1).(0) <- true;
  let cells = Array.make_matrix w h None in
  let try_union x y nx ny =
    if 0 <= nx && nx < w && 0 <= ny && ny < h && cur_grid.(ny).(nx) then
      ignore (UnionFind.union (Option.get cells.(y).(x)) (Option.get cells.(ny).(nx)))
  in

  List.iter (fun (x, y) -> cells.(y).(x) <- Some (UnionFind.make (x, y))) points;
  for x = 1 to w - 2 do
    cells.(0).(x) <- Some (UnionFind.make (x, 0));
    cells.(h - 1).(x) <- Some (UnionFind.make (x, h - 1));
  done;
  for y = 1 to h - 2 do
    cells.(y).(0) <- Some (UnionFind.make (0, y));
    cells.(y).(w - 1) <- Some (UnionFind.make (w - 1, y));
  done;
  cells.(0).(w - 1) <- Some (UnionFind.make (w - 1, 0));
  cells.(h - 1).(0) <- Some (UnionFind.make (0, h - 1));
  for x = 1 to w - 1 do
    try_union x 0 (x - 1) 0;
    try_union x (h - 1) (x - 1) (h - 1);
  done;
  for y = 1 to h - 1 do
    try_union 0 y 0 (y - 1);
    try_union (w - 1) y (w - 1) (y - 1);
  done;

  let first_blocking = List.find_index (fun (x, y) ->
    cur_grid.(y).(x) <- true;
    for dx = -1 to 1 do
      for dy = -1 to 1 do
        if dx <> 0 || dy <> 0 then
          try_union x y (x + dx) (y + dy)
      done
    done;
    UnionFind.eq (Option.get cells.(1).(0)) (Option.get cells.(0).(1))
  ) points in
  match first_blocking with
  | None -> Printf.printf "No blocking wall found\n"
  | Some i -> let p = List.nth points i in Printf.printf "%d,%d\n" (fst p - 1) (snd p - 1)
