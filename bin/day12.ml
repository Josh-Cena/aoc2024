let mark_regions data =
  let mat =
    Array.of_list
      (List.map (fun line -> Array.of_seq (String.to_seq line)) data)
  in
  let height = Array.length mat in
  let width = Array.length mat.(0) in
  let regions = Array.map (Array.map (fun _ -> -1)) mat in
  let region_id = ref 0 in
  let flood_fill x y =
    let queue = Queue.create () in
    Queue.push (x, y) queue;
    regions.(y).(x) <- !region_id;
    while not (Queue.is_empty queue) do
      let cx, cy = Queue.pop queue in
      let neighbors =
        [ (cx - 1, cy); (cx + 1, cy); (cx, cy - 1); (cx, cy + 1) ]
      in
      List.iter
        (fun (nx, ny) ->
          if nx >= 0 && nx < width && ny >= 0 && ny < height then
            if regions.(ny).(nx) = -1 && mat.(ny).(nx) = mat.(cy).(cx) then begin
              regions.(ny).(nx) <- !region_id;
              Queue.push (nx, ny) queue
            end)
        neighbors
    done
  in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      if regions.(y).(x) = -1 then begin
        flood_fill x y;
        region_id := !region_id + 1
      end
    done
  done;
  (regions, !region_id, width, height)

let solve1 data =
  let regions, region_count, width, height = mark_regions data in
  let areas = Array.make region_count 0 in
  let perimeters = Array.make region_count 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let rid = regions.(y).(x) in
      areas.(rid) <- areas.(rid) + 1;
      let neighbors = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ] in
      List.iter
        (fun (nx, ny) ->
          if
            nx < 0 || nx >= width || ny < 0 || ny >= height
            || regions.(ny).(nx) <> rid
          then perimeters.(rid) <- perimeters.(rid) + 1)
        neighbors
    done
  done;
  let types = List.init region_count (fun i -> areas.(i) * perimeters.(i)) in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 types)

let solve2 data =
  let regions, region_count, width, height = mark_regions data in
  let areas = Array.make region_count 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let rid = regions.(y).(x) in
      areas.(rid) <- areas.(rid) + 1
    done
  done;
  (* For each region: count corners. A reflex corner must be different from its
     diagonal neighbor but the same from both orthogonal neighbors next to that
     diagonal neighbor. A convex corner must be different from two adjacent
     orthogonal neighbors. E.g.:

      n1 n2 n3
      n4  c n5
      n6 n7 n8
        
     c should either be different from n2 and n4 or different from c1 but
     same as n2 and n4 *)
  let corners = Array.make region_count 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let rid = regions.(y).(x) in
      let c = regions.(y).(x) in
      let get_neighbor dx dy =
        if x + dx >= 0 && x + dx < width && y + dy >= 0 && y + dy < height then
          regions.(y + dy).(x + dx)
        else -1
      in
      List.iter
        (fun (dx, dy) ->
          let o1 = get_neighbor dx 0 in
          let o2 = get_neighbor 0 dy in
          let d = get_neighbor dx dy in
          if (c <> o1 && c <> o2) || (c <> d && c = o1 && c = o2) then
            corners.(rid) <- corners.(rid) + 1)
        [ (-1, -1); (-1, 1); (1, -1); (1, 1) ]
    done
  done;
  let types = List.init region_count (fun i -> areas.(i) * corners.(i)) in
  Printf.printf "%d\n" (List.fold_left ( + ) 0 types)
