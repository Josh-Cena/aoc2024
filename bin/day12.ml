let solve1 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_seq (String.to_seq line)) data) in
  let height = Array.length mat in
  let width = Array.length (mat.(0)) in
  let regions = Array.map (Array.map (fun _ -> -1)) mat in
  let region_id = ref 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      if regions.(y).(x) = -1 then begin
        let queue = Queue.create () in
        Queue.push (x, y) queue;
        regions.(y).(x) <- !region_id;
        while not (Queue.is_empty queue) do
          let (cx, cy) = Queue.pop queue in
          let neighbors = [(cx - 1, cy); (cx + 1, cy); (cx, cy - 1); (cx, cy + 1)] in
          List.iter (fun (nx, ny) ->
            if nx >= 0 && nx < width && ny >= 0 && ny < height then
              if regions.(ny).(nx) = -1 && mat.(ny).(nx) = mat.(cy).(cx) then begin
                regions.(ny).(nx) <- !region_id;
                Queue.push (nx, ny) queue
              end
          ) neighbors
        done;
        region_id := !region_id + 1
      end
    done
  done;
  let areas = Array.make !region_id 0 in
  let perimeters = Array.make !region_id 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let rid = regions.(y).(x) in
      areas.(rid) <- areas.(rid) + 1;
      let neighbors = [(x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1)] in
      List.iter (fun (nx, ny) ->
        if nx < 0 || nx >= width || ny < 0 || ny >= height ||
           regions.(ny).(nx) <> rid then
          perimeters.(rid) <- perimeters.(rid) + 1
      ) neighbors
    done
  done;
  let types = List.init !region_id (fun i -> areas.(i) * perimeters.(i)) in
  Printf.printf "%d\n" (List.fold_left (+) 0 types)

let solve2 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_seq (String.to_seq line)) data) in
  let height = Array.length mat in
  let width = Array.length (mat.(0)) in
  let regions = Array.map (Array.map (fun _ -> -1)) mat in
  let region_id = ref 0 in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      if regions.(y).(x) = -1 then begin
        let queue = Queue.create () in
        Queue.push (x, y) queue;
        regions.(y).(x) <- !region_id;
        while not (Queue.is_empty queue) do
          let (cx, cy) = Queue.pop queue in
          let neighbors = [(cx - 1, cy); (cx + 1, cy); (cx, cy - 1); (cx, cy + 1)] in
          List.iter (fun (nx, ny) ->
            if nx >= 0 && nx < width && ny >= 0 && ny < height then
              if regions.(ny).(nx) = -1 && mat.(ny).(nx) = mat.(cy).(cx) then begin
                regions.(ny).(nx) <- !region_id;
                Queue.push (nx, ny) queue
              end
          ) neighbors
        done;
        region_id := !region_id + 1
      end
    done
  done;
  let areas = Array.make !region_id 0 in
  (* For each region: count transitions. Edge for color X starts when the current
      cell is X and the previous cell is not X (or out of bounds), or the previous
      cell is X but the cell above that is also X. It ends when the next cell
      is no longer X, or the cell above that is also X. *)
  let transitions = Array.make !region_id 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let rid = regions.(y).(x) in
      areas.(rid) <- areas.(rid) + 1;
    done
  done;
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let rid = regions.(y).(x) in
      (*
        n1 n2 n3
        n4  c n5
        n6 n7 n8
      *)
      let c = regions.(y).(x) in
      let (n1, n2, n3, n4, n5, n6, n7, n8) =
        ((if x > 0 && y > 0 then regions.(y - 1).(x - 1) else -1),
         (if y > 0 then regions.(y - 1).(x) else -1),
         (if x < width - 1 && y > 0 then regions.(y - 1).(x + 1) else -1),
         (if x > 0 then regions.(y).(x - 1) else -1),
         (if x < width - 1 then regions.(y).(x + 1) else -1),
         (if x > 0 && y < height - 1 then regions.(y + 1).(x - 1) else -1),
         (if y < height - 1 then regions.(y + 1).(x) else -1),
         (if x < width - 1 && y < height - 1 then regions.(y + 1).(x + 1) else -1)) in
      (* Top start/end *)
      if c <> n2 && (c <> n4 || n4 == n1) then
        transitions.(rid) <- transitions.(rid) + 1;
      if c <> n2 && (c <> n5 || n5 == n3) then
        transitions.(rid) <- transitions.(rid) + 1;
      (* Bottom start/end *)
      if c <> n7 && (c <> n4 || n4 == n6) then
        transitions.(rid) <- transitions.(rid) + 1;
      if c <> n7 && (c <> n5 || n5 == n8) then
        transitions.(rid) <- transitions.(rid) + 1;
      (* Left start/end *)
      if c <> n4 && (c <> n2 || n2 == n1) then
        transitions.(rid) <- transitions.(rid) + 1;
      if c <> n4 && (c <> n7 || n7 == n6) then
        transitions.(rid) <- transitions.(rid) + 1;
      (* Right start/end *)
      if c <> n5 && (c <> n2 || n2 == n3) then
        transitions.(rid) <- transitions.(rid) + 1;
      if c <> n5 && (c <> n7 || n7 == n8) then
        transitions.(rid) <- transitions.(rid) + 1;
    done
  done;
  let types = List.init !region_id (fun i -> areas.(i) * transitions.(i) / 2) in
  Printf.printf "%d\n" (List.fold_left (+) 0 types)
