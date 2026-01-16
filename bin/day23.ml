module NodeSet = Set.Make (String)

let parse_graph data =
  let adj = Hashtbl.create 100 in
  List.iter
    (fun line ->
      let parts = String.split_on_char '-' line in
      let u = List.nth parts 0 in
      let v = List.nth parts 1 in
      let neighbors_u =
        match Hashtbl.find_opt adj u with Some s -> s | None -> NodeSet.empty
      in
      let neighbors_v =
        match Hashtbl.find_opt adj v with Some s -> s | None -> NodeSet.empty
      in
      Hashtbl.replace adj u (NodeSet.add v neighbors_u);
      Hashtbl.replace adj v (NodeSet.add u neighbors_v))
    data;
  adj

let solve1 data =
  let adj = parse_graph data in
  let triangles = ref 0 in
  Hashtbl.iter
    (fun u neighbors_u ->
      NodeSet.iter
        (fun v ->
          Hashtbl.iter
            (fun w neighbors_w ->
              if
                List.exists (String.starts_with ~prefix:"t") [ u; v; w ]
                && NodeSet.mem u neighbors_w && NodeSet.mem v neighbors_w
              then incr triangles)
            adj)
        neighbors_u)
    adj;
  Printf.printf "%d\n" (!triangles / 6)

let max_clique adj =
  (* Choose a pivot u in (P ∪ X) maximizing |P ∩ N(u)| *)
  let choose_pivot p x =
    let candidates = NodeSet.union p x in
    NodeSet.fold
      (fun u (best_u, best_deg) ->
        let deg = Hashtbl.find adj u |> NodeSet.inter p |> NodeSet.cardinal in
        if Option.is_none best_u || deg > best_deg then (Some u, deg)
        else (best_u, best_deg))
      candidates (None, -1)
    |> fst |> Option.get
  in

  (* https://en.wikipedia.org/wiki/Bron%E2%80%93Kerbosch_algorithm *)
  let rec search_max r p x best =
    let best_size = NodeSet.cardinal best in
    if NodeSet.cardinal r + NodeSet.cardinal p <= best_size then
      (* Can't beat the best *)
      best
    else if NodeSet.is_empty p && NodeSet.is_empty x then
      (* if P and X are both empty then *)
      (* Report R as a maximal clique; update best if R is larger *)
      if NodeSet.cardinal r > best_size then r else best
    else
      (* choose a pivot vertex u in P ⋃ X *)
      let u = choose_pivot p x in
      let nu = Hashtbl.find adj u in
      (* for each vertex v in P \ N(u) do *)
      let _, _, best =
        NodeSet.fold
          (fun v (p, x, best) ->
            let nv = Hashtbl.find adj v in
            (* BronKerbosch2(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v)) *)
            let best' =
              search_max (NodeSet.add v r) (NodeSet.inter p nv)
                (NodeSet.inter x nv) best
            in
            (* P := P \ {v}; X := X ⋃ {v} *)
            (NodeSet.remove v p, NodeSet.add v x, best'))
          (NodeSet.diff p nu) (p, x, best)
      in
      best
  in

  let nodes =
    Hashtbl.fold (fun k _ acc -> NodeSet.add k acc) adj NodeSet.empty
  in
  search_max NodeSet.empty nodes NodeSet.empty NodeSet.empty

let solve2 data =
  let adj = parse_graph data in
  let clique = max_clique adj in
  Printf.printf "%s\n" (NodeSet.to_list clique |> String.concat ",")
