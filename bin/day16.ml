type state = { x: int; y: int; dirx: int; diry: int }

let edges w h board st =
  List.filter (fun (st', _) ->
    st'.x >= 0 && st'.x < w && st'.y >= 0 && st'.y < h && board.(st'.y).(st'.x) <> '#')
    [ ({ st with x = st.x + st.dirx; y = st.y + st.diry }, 1);
      ({ st with dirx = -st.diry; diry = st.dirx }, 1000);
      ({ st with dirx = st.diry; diry = -st.dirx }, 1000) ]

let edges_rev w h board st =
  List.filter (fun (st', _) ->
    st'.x >= 0 && st'.x < w && st'.y >= 0 && st'.y < h && board.(st'.y).(st'.x) <> '#')
    [ ({ st with x = st.x - st.dirx; y = st.y - st.diry }, 1);
      ({ st with dirx = -st.diry; diry = st.dirx }, 1000);
      ({ st with dirx = st.diry; diry = -st.dirx }, 1000) ]

module StateOrder = struct
  type t = state * int
  let compare (_, c1) (_, c2) = Int.compare c1 c2
end

module MinQueue = Pqueue.MakeMin(StateOrder)

let dijkstra mat rev s =
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  let pq = MinQueue.create () in
  let dist = Hashtbl.create (width * height * 4) in
  let prev = Hashtbl.create (width * height * 4) in
  MinQueue.add pq (s, 0);
  Hashtbl.add dist s 0;
  while not (MinQueue.is_empty pq) do
    let (u, _) = match MinQueue.pop_min pq with
      | Some v -> v
      | None -> failwith "unreachable" in
    match Hashtbl.find_opt dist u with
    | None -> ()
    | Some u_dist ->
      let neighbors = (if rev then edges_rev else edges) width height mat u in
      List.iter (fun (v, weight) ->
        let is_better = begin match Hashtbl.find_opt dist v with
        | None -> true
        | Some v_dist -> u_dist + weight < v_dist
        end in
        let is_equiv = begin match Hashtbl.find_opt dist v with
        | None -> false
        | Some v_dist -> u_dist + weight = v_dist
        end in
        if is_better then begin
          Hashtbl.replace dist v (u_dist + weight);
          Hashtbl.replace prev v [u];
          MinQueue.add pq (v, u_dist + weight)
        end else if is_equiv then
          Hashtbl.replace prev v (u :: Hashtbl.find prev v)
      ) neighbors
  done;
  (dist, prev)

let solve1 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_seq (String.to_seq line)) data) in
  let s = ref { x = 0; y = 0; dirx = 1; diry = 0 } in
  let e = ref { x = 0; y = 0; dirx = 0; diry = 0 } in
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      if mat.(y).(x) = 'S' then
        s := { x; y; dirx = 1; diry = 0 }
      else if mat.(y).(x) = 'E' then
        e := { x; y; dirx = 0; diry = 0 }
    done
  done;
  let (dist, _) = dijkstra mat false !s in
  let e1_dist = Hashtbl.find dist { !e with dirx = 1; diry = 0 } in
  let e2_dist = Hashtbl.find dist { !e with dirx = 0; diry = 1 } in
  let e3_dist = Hashtbl.find dist { !e with dirx = -1; diry = 0 } in
  let e4_dist = Hashtbl.find dist { !e with dirx = 0; diry = -1 } in
  let res = List.fold_left min max_int [e1_dist; e2_dist; e3_dist; e4_dist] in
  Printf.printf "%d\n" res

let solve2 data =
  let mat = Array.of_list (List.map (fun line ->
      Array.of_seq (String.to_seq line)) data) in
  let s = ref { x = 0; y = 0; dirx = 1; diry = 0 } in
  let e = ref { x = 0; y = 0; dirx = 0; diry = 0 } in
  let width = Array.length mat.(0) in
  let height = Array.length mat in
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      if mat.(y).(x) = 'S' then
        s := { x; y; dirx = 1; diry = 0 }
      else if mat.(y).(x) = 'E' then
        e := { x; y; dirx = 0; diry = 0 }
    done
  done;
  let (dist_s, _) = dijkstra mat false !s in
  let e1 = { !e with dirx = 1; diry = 0 } in
  let e2 = { !e with dirx = 0; diry = 1 } in
  let e3 = { !e with dirx = -1; diry = 0 } in
  let e4 = { !e with dirx = 0; diry = -1 } in
  let e1_dist = Hashtbl.find dist_s e1 in
  let e2_dist = Hashtbl.find dist_s e2 in
  let e3_dist = Hashtbl.find dist_s e3 in
  let e4_dist = Hashtbl.find dist_s e4 in
  let min_dist = List.fold_left min max_int [e1_dist; e2_dist; e3_dist; e4_dist] in
  let dist_es = [ (e1, e1_dist); (e2, e2_dist); (e3, e3_dist); (e4, e4_dist) ]
      |> List.filter (fun (_, d) -> d = min_dist)
      |> List.map fst
      |> List.map (dijkstra mat true)
      |> List.map fst in
  let optimal_states = Hashtbl.fold (fun st d acc ->
      if List.exists (fun dist_e -> match Hashtbl.find_opt dist_e st with
          | Some dist -> dist + d = min_dist
          | None -> false) dist_es then
        (st.x, st.y) :: acc
      else
        acc) dist_s [] in
  let st_uniq = List.sort_uniq compare optimal_states in
  Printf.printf "%d\n" (List.length st_uniq)
